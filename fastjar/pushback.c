/* $Id: pushback.c,v 1.2 2000/12/14 18:45:35 ghazi Exp $

   $Log: pushback.c,v $
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

   Revision 1.2  2000/08/23 19:42:17  cory
   Added support for more Unix platforms.  The following code has been hacked
   to work on AIX, Solaris, True 64, and HP-UX.
   Added bigendian check.  Probably works on most big and little endian platforms
   now.

   Revision 1.1.1.1  1999/12/06 03:09:13  toast
   initial checkin..



   Revision 1.1  1999/05/10 08:32:37  burnsbr
   Initial revision

*/

/*
  pushback.c - code for a pushback buffer to handle file I/O
  Copyright (C) 1999  Bryan Burns
  
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

#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include "jartool.h"
#include "pushback.h"

void pb_init(pb_file *pbf, int fd){
  pbf->fd = fd;
  pbf->next = pbf->pb_buff;
  pbf->buff_amt = 0;
}

int pb_push(pb_file *pbf, void *buff, int amt){
  int in_amt;
  int wrap = 0;

#ifdef DEBUG
  printf("%d bytes being pushed back to the buffer\n", amt);
#endif

  /* determine how much we can take */
  if((int)(RDSZ - pbf->buff_amt) < amt)
    in_amt = RDSZ - pbf->buff_amt;
  else
    in_amt = amt;

  if(in_amt == 0)
    return 0;

  /* figure out if we need to wrap around, and if so, by how much */
  if(((pbf->pb_buff + RDSZ) - pbf->next) < in_amt)
    wrap = in_amt - ((pbf->pb_buff + RDSZ) - pbf->next);

  /* write everything up til the end of the buffer */
  memcpy(pbf->next, buff, (in_amt - wrap));

  /* finish writing what's wrapped around */
  memcpy(pbf->pb_buff, ((char *)buff + (in_amt - wrap)), wrap);
         
  /* update the buff_amt field */
  pbf->buff_amt += in_amt;

#ifdef DEBUG
  printf("%d bytes we can't accept\n", (amt - in_amt));
#endif

  return in_amt;
}


int pb_read(pb_file *pbf, void *buff, int amt){
  int out_amt = 0;
  int wrap = 0;
  void *bp = buff;
  int tmp;

#ifdef DEBUG
  printf("%d bytes requested from us\n", amt);
#endif
  while(out_amt < amt){
    /* if our push-back buffer contains some data */
    if(pbf->buff_amt > 0){
      
#ifdef DEBUG
      printf("giving data from buffer\n");
#endif
      
      /* calculate how much we can actually give the caller */
      if( (amt - out_amt) < (int)pbf->buff_amt )
        tmp = (amt - out_amt);
      else
        tmp = pbf->buff_amt;
      
      /* Determine if we're going to need to wrap around the buffer */
      if(tmp > ((pbf->pb_buff + RDSZ) - pbf->next))
        wrap = tmp - ((pbf->pb_buff + RDSZ) - pbf->next);
      
      memcpy(bp, pbf->next, (tmp - wrap));
      bp = &(((char *)bp)[tmp - wrap]);
      
      /* If we need to wrap, read from the start of the buffer */
      if(wrap > 0){
        memcpy(bp, pbf->pb_buff, wrap);
        bp = &(((char *)bp)[wrap]);
      }
      
      /* update the buff_amt field */
      pbf->buff_amt -= tmp;
      pbf->next += tmp;
      
#ifdef DEBUG
      printf("%d bytes remaining in buffer\n", pbf->buff_amt);
#endif
      
      /* if the buffer is empty, reset the next header to the front of the
         buffer so subsequent pushbacks/reads won't have to wrap */
      if(pbf->buff_amt == 0)
        pbf->next = pbf->pb_buff;
      
      out_amt += tmp;

    } else {
#ifdef DEBUG
      printf("Reading from file..\n");
#endif
      
      /* The pushback buffer was empty, so we just need to read from the file */
      tmp = read(pbf->fd, bp, (amt - out_amt));
      if(tmp == 0)
        break;
      else
        out_amt += tmp;
      
      bp = &(((char *)bp)[tmp]);
    }
  }

#ifdef DEBUG
  printf("managed to read %d bytes\n", out_amt);
#endif
  return out_amt;
}
