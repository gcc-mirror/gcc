/* $Id: compress.c,v 1.2 2000/12/14 18:45:35 ghazi Exp $

   $Log: compress.c,v $
   Revision 1.2  2000/12/14 18:45:35  ghazi
   Warning fixes:

   	* compress.c: Include stdlib.h and compress.h.
   	(rcsid): Delete.
   	(report_str_error): Make static.
   	(ez_inflate_str): Delete unused variable.  Add parens in if-stmt.
   	(hrd_inflate_str): Likewise.

   	* compress.h (init_compression, end_compression, init_inflation,
   	end_inflation): Prototype void arguments.

   	* dostime.c (rcsid): Delete.

   	* jargrep.c: Include ctype.h, stdlib.h, zlib.h and compress.h.
   	Make functions static.  Cast ctype function argument to `unsigned
   	char'.  Add parens in if-stmts.  Constify.
   	(Usage): Change into a macro.
   	(jargrep): Remove unused parameter.

   	* jartool.c: Constify.  Add parens in if-stmts.  Align
   	signed/unsigned char pointers in functions calls using casts.
   	(rcsid): Delete.
   	(list_jar): Fix printf format specifier.
   	(usage): Chop long string into bits.  Reformat.

   	* pushback.c (rcsid): Delete.

   Revision 1.1  2000/12/09 03:08:23  apbianco
   2000-12-08  Alexandre Petit-Bianco  <apbianco@cygnus.com>

           * fastjar: Imported.

   Revision 1.7  2000/09/13 14:02:02  cory
   Reformatted some of the code to more closly match the layout of the orriginal
   fastjar utility.

   Revision 1.6  2000/09/12 22:29:36  cory
   Jargrep now seems to do what I want it to do.  Performs properly on Linux x86,
   will test some other platforms later.

   Revision 1.1.1.1  1999/12/06 03:09:16  toast
   initial checkin..



   Revision 1.7  1999/05/10 08:50:05  burnsbr
   *** empty log message ***

   Revision 1.6  1999/05/10 08:38:44  burnsbr
   *** empty log message ***

   Revision 1.5  1999/05/10 08:30:29  burnsbr
   added inflation code

   Revision 1.4  1999/04/27 10:03:33  burnsbr
   added configure support

   Revision 1.3  1999/04/26 02:35:32  burnsbr
   compression now works.. yahoo

   Revision 1.2  1999/04/23 12:01:59  burnsbr
   added licence stuff.

   Revision 1.1  1999/04/23 11:58:25  burnsbr
   Initial revision


*/

/*
  compress.c - code for handling deflation
  Copyright (C) 1999  Bryan Burns
  Copyright (C) 2004  Free Software Foundation, Inc.
  
  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

#include "config.h"

#include <zlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include <sys/types.h>

#include "jartool.h"
#include "pushback.h"
#include "compress.h"
#include "shift.h"

int write_data (int, void *, size_t, struct zipentry *);

extern int seekable;
extern off_t end_of_entries;

static z_stream zs;

void init_compression(){

  memset(&zs, 0, sizeof(z_stream));

  zs.zalloc = Z_NULL;
  zs.zfree = Z_NULL;
  zs.opaque = Z_NULL;

  /* Why -MAX_WBITS?  zlib has an undocumented feature, where if the windowbits
     parameter is negative, it omits the zlib header, which seems to kill
     any other zip/unzip program.  This caused me SO much pain.. */
  if(deflateInit2(&zs, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -MAX_WBITS, 
                  9, Z_DEFAULT_STRATEGY) != Z_OK){
    
    fprintf(stderr, "Error initializing deflation!\n");
    exit(1);
  }
}

int
write_data (int fd, void *buf, size_t len, struct zipentry *ze)
{
#ifdef WITH_SHIFT_DOWN
  struct zipentry *next = NULL;
  off_t here = lseek (fd, 0, SEEK_CUR);
  /*
   * If we are updating and there is not enough space before the next
   * entry, expand the file.
   */
  if (ze)
    {
      next = ze->next_entry;
      if (next && here + len >= next->offset)
	{
	  if (shift_down (fd, next->offset, (here + len) - next->offset, next))
	    {
	      perror ("can't expand file");
	      exit (1);
	    }
	}
    }
#endif /* WITH_SHIFT_DOWN */

  return write (fd, buf, len);
}

int compress_file(int in_fd, int out_fd, struct zipentry *ze,
		  struct zipentry *existing)
{
  Bytef in_buff[RDSZ];
  Bytef out_buff[RDSZ];
  unsigned int rdamt, wramt;
  unsigned long tr = 0;
  int rtval;

  rdamt = 0;

  zs.avail_in = 0;
  zs.next_in = in_buff;
  
  zs.next_out = out_buff;
  zs.avail_out = (uInt)RDSZ;
  
  ze->crc = crc32(0L, Z_NULL, 0); 
  
  for(; ;){
    
    /* If deflate is out of input, fill the input buffer for it */
    if(zs.avail_in == 0 && zs.avail_out > 0){
      if((rtval = read(in_fd, in_buff, RDSZ)) == 0)
        break;

      if(rtval == -1){
        perror("read");
        exit(1);
      }

      rdamt = rtval;

      /* compute the CRC while we're at it */
      ze->crc = crc32(ze->crc, in_buff, rdamt); 

      /* update the total amount read sofar */
      tr += rdamt;

      zs.next_in = in_buff;
      zs.avail_in = rdamt;
    }
    
    /* deflate the data */
    if(deflate(&zs, 0) != Z_OK){
      fprintf(stderr, "Error deflating! %s:%d\n", __FILE__, __LINE__);
      exit(1);
    }
    
    /* If the output buffer is full, dump it to disk */
    if(zs.avail_out == 0){

      if (write_data (out_fd, out_buff, RDSZ, existing) != RDSZ)
	{
	  perror("write");
	  exit(1);
	}

      /* clear the output buffer */
      zs.next_out = out_buff;
      zs.avail_out = (uInt)RDSZ;
    }

  }
  
  /* If we have any data waiting in the buffer after we're done with the file
     we can flush it */
  if(zs.avail_out < RDSZ){

    wramt = RDSZ - zs.avail_out;

    if (write_data (out_fd, out_buff, wramt, existing) != (int)wramt)
      {
	perror("write");
	exit(1);
      }
    /* clear the output buffer */
    zs.next_out = out_buff;
    zs.avail_out = (uInt)RDSZ;
  }
  

  /* finish deflation.  This purges zlib's internal data buffers */
  while(deflate(&zs, Z_FINISH) == Z_OK){
    wramt = RDSZ - zs.avail_out;

    if (write_data (out_fd, out_buff, wramt, existing) != (int)wramt)
      {
	perror("write");
	exit(1);
      }

    zs.next_out = out_buff;
    zs.avail_out = (uInt)RDSZ;
  }

  /* If there's any data left in the buffer, write it out */
  if(zs.avail_out != RDSZ){
    wramt = RDSZ - zs.avail_out;

    if (write_data (out_fd, out_buff, wramt, existing) != (int)wramt)
      {
	perror("write");
	exit(1);
      }
  }

  /* update fastjar's entry information */
  ze->usize = (ub4)zs.total_in;
  ze->csize = (ub4)zs.total_out;

  /* Reset the deflation for the next time around */
  if(deflateReset(&zs) != Z_OK){
    fprintf(stderr, "Error resetting deflation\n");
    exit(1);
  }
  
  return 0;
}

void end_compression(){
  int rtval;

  /* Oddly enough, zlib always returns Z_DATA_ERROR if you specify no
     zlib header.  Go fig. */
  if((rtval = deflateEnd(&zs)) != Z_OK && rtval != Z_DATA_ERROR){
    fprintf(stderr, "Error calling deflateEnd\n");
    fprintf(stderr, "error: (%d) %s\n", rtval, zs.msg);
    exit(1);
  }
}


void init_inflation(){

  memset(&zs, 0, sizeof(z_stream));
    
  zs.zalloc = Z_NULL;
  zs.zfree = Z_NULL;
  zs.opaque = Z_NULL;
  
  if(inflateInit2(&zs, -15) != Z_OK){
    fprintf(stderr, "Error initializing deflation!\n");
    exit(1);
  }

}

int inflate_file(pb_file *pbf, int out_fd, struct zipentry *ze){
  Bytef in_buff[RDSZ];
  Bytef out_buff[RDSZ];
  unsigned int rdamt;
  int rtval;
  ub4 crc = 0;

  zs.avail_in = 0;

  crc = crc32(crc, NULL, 0); /* initialize crc */

  /* loop until we've consumed all the compressed data */
  for(;;){
    
    if(zs.avail_in == 0){
      if((rdamt = pb_read(pbf, in_buff, RDSZ)) == 0)
        break;
      else if((int)rdamt < 0){
        perror("read");
        exit(1);
      }

#ifdef DEBUG
      printf("%d bytes read\n", rdamt);
#endif

      zs.next_in = in_buff;
      zs.avail_in = rdamt;
    }

    zs.next_out = out_buff;
    zs.avail_out = RDSZ;
    
    if((rtval = inflate(&zs, 0)) != Z_OK){
      if(rtval == Z_STREAM_END){
#ifdef DEBUG
        printf("end of stream\n");
#endif
        if(zs.avail_out != RDSZ){
          crc = crc32(crc, out_buff, (RDSZ - zs.avail_out));

          if(out_fd >= 0)
            if(write(out_fd, out_buff, (RDSZ - zs.avail_out)) != 
               (int)(RDSZ - zs.avail_out)){
              perror("write");
              exit(1);
            }
        }
        
        break;
      } else {
        fprintf(stderr, "Error inflating file! (%d)\n", rtval);
        exit(1);
      }
    } else {
      if(zs.avail_out != RDSZ){
        crc = crc32(crc, out_buff, (RDSZ - zs.avail_out));

        if(out_fd >= 0)
          if(write(out_fd, out_buff, (RDSZ - zs.avail_out)) != 
             (int)(RDSZ - zs.avail_out)){
            perror("write");
            exit(1);
          }
        zs.next_out = out_buff;
        zs.avail_out = RDSZ;
      }
    }
  }
#ifdef DEBUG
  printf("done inflating\n");
#endif

#ifdef DEBUG
  printf("%d bytes left over\n", zs.avail_in);
#endif

#ifdef DEBUG    
  printf("CRC is %x\n", crc);
#endif

  ze->crc = crc;
  
  pb_push(pbf, zs.next_in, zs.avail_in);

  ze->usize = zs.total_out;

  inflateReset(&zs);
  return 0;
}

/*
Function name: report_str_error
args:	val	Error code returned from zlib.
purpose: Put out an error message corresponding to error code returned from zlib.
Be suitably cryptic seeing I don't really know exactly what these errors mean.
*/

static void report_str_error(int val) {
	switch(val) {
	case Z_STREAM_END:
		break;
	case Z_NEED_DICT:
		fprintf(stderr, "Need a dictionary?\n");
		exit(1);
	case Z_DATA_ERROR:
		fprintf(stderr, "Z_DATA_ERROR\n");
		exit(1);
	case Z_STREAM_ERROR:
		fprintf(stderr, "Z_STREAM_ERROR\n");
		exit(1);
	case Z_MEM_ERROR:
		fprintf(stderr, "Z_MEM_ERROR\n");
		exit(1);
	case Z_BUF_ERROR:
		fprintf(stderr, "Z_BUF_ERROR\n");
		exit(1);
	case Z_OK:
		break;
	default:
		fprintf(stderr, "Unknown behavior from inflate\n");
		exit(1);
	}
}

/*
Function name: ez_inflate_str
args:	pbf		Pointer to pushback handle for file.
		csize	Compressed size of embedded file.
		usize	Uncompressed size of embedded file.
purpose: Read in and decompress the contents of an embedded file and store it in a
byte array.
returns: Byte array of uncompressed embedded file.
*/

static Bytef *ez_inflate_str(pb_file *pbf, ub4 csize, ub4 usize) {
	Bytef *out_buff;
	Bytef *in_buff;
	unsigned int rdamt;

	if((zs.next_in = in_buff = (Bytef *) malloc(csize))) {
		if((zs.next_out = out_buff = (Bytef *) malloc(usize + 1))) { 
			if((rdamt = pb_read(pbf, zs.next_in, csize)) == csize) {
				zs.avail_in = csize;
				zs.avail_out = usize;
				report_str_error(inflate(&zs, 0));
				free(in_buff);
				inflateReset(&zs);
				out_buff[usize] = '\0';
			}
			else {
				fprintf(stderr, "Read failed on input file.\n");
				fprintf(stderr, "Tried to read %u but read %u instead.\n", csize, rdamt);
				free(in_buff);
				free(out_buff);
				exit(1);
			}
		}
		else {
			fprintf(stderr, "Malloc of out_buff failed.\n");
			fprintf(stderr, "Error: %s\n", strerror(errno));
			free(in_buff);
			exit(1);
		}
	}
	else {
		fprintf(stderr, "Malloc of in_buff failed.\n");
		fprintf(stderr, "Error: %s\n", strerror(errno));
		exit(1);
	}

	return out_buff;
}

/*
Function name: hrd_inflate_str
args:	pbf		Pointer to pushback handle for file.
		csize	Pointer to compressed size of embedded file.
		usize	Pointer to uncompressed size of embedded file.
purpose: Read and decompress an embedded file into a string.  Set csize and usize
accordingly.  This function does the reading for us in the case there is not size
information in the header for the embedded file.
returns: Byte array of the contents of the embedded file.
*/

static Bytef *hrd_inflate_str(pb_file *pbf, ub4 *csize, ub4 *usize) {
	Bytef *out_buff;
	Bytef *tmp;
	Bytef in_buff[RDSZ];
	unsigned int rdamt;
	int i;
	int zret;

	i = 1; 
	out_buff = NULL;
	zret = Z_OK;
	while(zret != Z_STREAM_END && (rdamt = pb_read(pbf, in_buff, RDSZ)))
	{
		zs.avail_in = rdamt;
		zs.avail_out = 0;
		zs.next_in = in_buff;
		do {
			if((tmp = (Bytef *) realloc(out_buff, (RDSZ * i) + 1))) {
				out_buff = tmp;
				zs.next_out = &(out_buff[(RDSZ * (i - 1)) - zs.avail_out]);
				zs.avail_out += RDSZ;
				i++;
			}
			else {
				fprintf(stderr, "Realloc of out_buff failed.\n");
				fprintf(stderr, "Error: %s\n", strerror(errno));
				exit(1);
			}
		} while((zret = inflate(&zs, 0)) == Z_OK);
		report_str_error(zret);
	}
	pb_push(pbf, zs.next_in, zs.avail_in);

	out_buff[(RDSZ * (i - 1)) - zs.avail_out] = '\0';
	*usize = zs.total_out;
	*csize = zs.total_in;

	inflateReset(&zs);

	return out_buff;
}

/*
Function name: inflate_string
args:	pbf		Pointer to pushback handle for file.
		csize	Pointer to compressed size of embedded file.  May be 0 if not set.
		usize	Pointer to uncompressed size of embedded file. May be 0 if not set.
purpose: Decide the easiest (in computer terms) methos of decompressing this embedded
file to a string.
returns: Pointer to a string containing the decompressed contents of the embedded file.
If csize and usize are not set set them to correct numbers.
*/

Bytef *inflate_string(pb_file *pbf, ub4 *csize, ub4 *usize) {
Bytef *ret_buf;

	if(*csize && *usize) ret_buf = ez_inflate_str(pbf, *csize, *usize);
	else ret_buf = hrd_inflate_str(pbf, csize, usize);

	return ret_buf;
}
