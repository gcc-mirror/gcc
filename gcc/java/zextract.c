/* Handle a .class file embedded in a .zip archive.
   This extracts a member from a .zip file, but does not handle
   uncompression (since that is not needed for classes.zip).

   Copyright (C) 1996, 97-99, 2000  Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com>, February 1996. */

#include "config.h"
#include "system.h"
#include "zipfile.h"

/* This stuff is partly based on the 28 August 1994 public release of the
Info-ZIP group's portable UnZip zipfile-extraction program (and related
utilities). */

/*************/
/*  Defines  */
/*************/

#define UNZIP
#define UNZIP_VERSION     20   /* compatible with PKUNZIP 2.0 */
#define VMS_UNZIP_VERSION 42   /* if OS-needed-to-extract is VMS:  can do */


#define ZSUFX             ".zip"
#define CENTRAL_HDR_SIG   "\113\001\002"   /* the infamous "PK" signature */
#define LOCAL_HDR_SIG     "\113\003\004"   /*  bytes, sans "P" (so unzip */
#define END_CENTRAL_SIG   "\113\005\006"   /*  executable not mistaken for */
#define EXTD_LOCAL_SIG    "\113\007\010"   /*  zipfile itself) */

#define STORED            0    /* compression methods */
#define SHRUNK            1
#define REDUCED1          2
#define REDUCED2          3
#define REDUCED3          4
#define REDUCED4          5
#define IMPLODED          6
#define TOKENIZED         7
#define DEFLATED          8
#define NUM_METHODS       9    /* index of last method + 1 */
/* don't forget to update list_files() appropriately if NUM_METHODS changes */

#define PK_OK             0    /* no error */
#define PK_COOL           0    /* no error */
#define PK_GNARLY         0    /* no error */
#define PK_WARN           1    /* warning error */
#define PK_ERR            2    /* error in zipfile */
#define PK_BADERR         3    /* severe error in zipfile */
#define PK_MEM            4    /* insufficient memory */
#define PK_MEM2           5    /* insufficient memory */
#define PK_MEM3           6    /* insufficient memory */
#define PK_MEM4           7    /* insufficient memory */
#define PK_MEM5           8    /* insufficient memory */
#define PK_NOZIP          9    /* zipfile not found */
#define PK_PARAM          10   /* bad or illegal parameters specified */
#define PK_FIND           11   /* no files found */
#define PK_DISK           50   /* disk full */
#define PK_EOF            51   /* unexpected EOF */

/*---------------------------------------------------------------------------
    True sizes of the various headers, as defined by PKWARE--so it is not
    likely that these will ever change.  But if they do, make sure both these
    defines AND the typedefs below get updated accordingly.
  ---------------------------------------------------------------------------*/
#define LREC_SIZE     26    /* lengths of local file headers, central */
#define CREC_SIZE     42    /*  directory headers, and the end-of-    */
#define ECREC_SIZE    18    /*  central-dir record, respectively      */


#ifndef SEEK_SET
#  define SEEK_SET  0
#  define SEEK_CUR  1
#  define SEEK_END  2
#endif

/**************/
/*  Typedefs  */
/**************/

typedef char              boolean;
typedef unsigned char     uch;  /* code assumes unsigned bytes; these type-  */
typedef unsigned short    ush;  /*  defs replace byte/UWORD/ULONG (which are */
typedef unsigned long     ulg;  /*  predefined on some systems) & match zip  */

/*---------------------------------------------------------------------------
    Zipfile layout declarations.  If these headers ever change, make sure the
    xxREC_SIZE defines (above) change with them!
  ---------------------------------------------------------------------------*/

   typedef uch   local_byte_hdr[ LREC_SIZE ];
#      define L_VERSION_NEEDED_TO_EXTRACT_0     0
#      define L_VERSION_NEEDED_TO_EXTRACT_1     1
#      define L_GENERAL_PURPOSE_BIT_FLAG        2
#      define L_COMPRESSION_METHOD              4
#      define L_LAST_MOD_FILE_TIME              6
#      define L_LAST_MOD_FILE_DATE              8
#      define L_CRC32                           10
#      define L_COMPRESSED_SIZE                 14
#      define L_UNCOMPRESSED_SIZE               18
#      define L_FILENAME_LENGTH                 22
#      define L_EXTRA_FIELD_LENGTH              24

  typedef uch   cdir_byte_hdr[ CREC_SIZE ];
#      define C_VERSION_MADE_BY_0               0
#      define C_VERSION_MADE_BY_1               1
#      define C_VERSION_NEEDED_TO_EXTRACT_0     2
#      define C_VERSION_NEEDED_TO_EXTRACT_1     3
#      define C_GENERAL_PURPOSE_BIT_FLAG        4
#      define C_COMPRESSION_METHOD              6
#      define C_LAST_MOD_FILE_TIME              8
#      define C_LAST_MOD_FILE_DATE              10
#      define C_CRC32                           12
#      define C_COMPRESSED_SIZE                 16
#      define C_UNCOMPRESSED_SIZE               20
#      define C_FILENAME_LENGTH                 24
#      define C_EXTRA_FIELD_LENGTH              26
#      define C_FILE_COMMENT_LENGTH             28
#      define C_DISK_NUMBER_START               30
#      define C_INTERNAL_FILE_ATTRIBUTES        32
#      define C_EXTERNAL_FILE_ATTRIBUTES        34
#      define C_RELATIVE_OFFSET_LOCAL_HEADER    38

   typedef uch   ec_byte_rec[ ECREC_SIZE+4 ];
/*     define SIGNATURE                         0   space-holder only */
#      define NUMBER_THIS_DISK                  4
#      define NUM_DISK_WITH_START_CENTRAL_DIR   6
#      define NUM_ENTRIES_CENTRL_DIR_THS_DISK   8
#      define TOTAL_ENTRIES_CENTRAL_DIR         10
#      define SIZE_CENTRAL_DIRECTORY            12
#      define OFFSET_START_CENTRAL_DIRECTORY    16
#      define ZIPFILE_COMMENT_LENGTH            20


   typedef struct local_file_header {                 /* LOCAL */
       uch version_needed_to_extract[2];
       ush general_purpose_bit_flag;
       ush compression_method;
       ush last_mod_file_time;
       ush last_mod_file_date;
       ulg crc32;
       ulg csize;
       ulg ucsize;
       ush filename_length;
       ush extra_field_length;
   } local_file_hdr;

   typedef struct central_directory_file_header {     /* CENTRAL */
       uch version_made_by[2];
       uch version_needed_to_extract[2];
       ush general_purpose_bit_flag;
       ush compression_method;
       ush last_mod_file_time;
       ush last_mod_file_date;
       ulg crc32;
       ulg csize;
       ulg ucsize;
       ush filename_length;
       ush extra_field_length;
       ush file_comment_length;
       ush disk_number_start;
       ush internal_file_attributes;
       ulg external_file_attributes;
       ulg relative_offset_local_header;
   } cdir_file_hdr;

   typedef struct end_central_dir_record {            /* END CENTRAL */
       ush number_this_disk;
       ush num_disk_with_start_central_dir;
       ush num_entries_centrl_dir_ths_disk;
       ush total_entries_central_dir;
       ulg size_central_directory;
       ulg offset_start_central_directory;
       ush zipfile_comment_length;
   } ecdir_rec;


/************/
/*  Macros  */
/************/

#ifndef MAX
#  define MAX(a,b)   ((a) > (b) ? (a) : (b))
#endif
#ifndef MIN
#  define MIN(a,b)   ((a) < (b) ? (a) : (b))
#endif


/***********************/
/* Prototypes          */
/***********************/

static ush makeword PARAMS ((const uch *));
static ulg makelong PARAMS ((const uch *));

/***********************/
/* Function makeword() */
/***********************/

static ush makeword(b)
    const uch *b;
{
    /*
     * Convert Intel style 'short' integer to non-Intel non-16-bit
     * host format.  This routine also takes care of byte-ordering.
     */
    return (ush)((b[1] << 8) | b[0]);
}


/***********************/
/* Function makelong() */
/***********************/

static ulg makelong(sig)
    const uch *sig;
{
    /*
     * Convert intel style 'long' variable to non-Intel non-16-bit
     * host format.  This routine also takes care of byte-ordering.
     */
    return (((ulg)sig[3]) << 24)
        + (((ulg)sig[2]) << 16)
        + (((ulg)sig[1]) << 8)
        + ((ulg)sig[0]);
}

int
read_zip_archive (zipf)
     register ZipFile *zipf;
{
  int i;
  int dir_last_pad;
  char *dir_ptr;
  char buffer[100];

  zipf->size = lseek (zipf->fd, 0L, SEEK_END);

  if (zipf->size < (ECREC_SIZE+4) || lseek (zipf->fd, (long)(-(ECREC_SIZE+4)), SEEK_CUR) <= 0)
    return -1;
  if (read (zipf->fd, buffer, ECREC_SIZE+4) != ECREC_SIZE+4)
    return -2;
  zipf->count = makeword(&buffer[TOTAL_ENTRIES_CENTRAL_DIR]);
  zipf->dir_size = makelong(&buffer[SIZE_CENTRAL_DIRECTORY]);
#define ALLOC xmalloc
  /* Allocate 1 more to allow appending '\0' to last filename. */
  zipf->central_directory = ALLOC (zipf->dir_size+1);
  if (lseek (zipf->fd, -(zipf->dir_size+ECREC_SIZE+4), SEEK_CUR) < 0)
    return -2;
  if (read (zipf->fd, zipf->central_directory, zipf->dir_size) < 0)
    return -2;

#ifdef TEST
  printf ("number_this_disk = %d\n", makeword(&buffer[NUMBER_THIS_DISK]));
  printf ("num_disk_with_start_central_dir = %d\n", makeword(&buffer[NUM_DISK_WITH_START_CENTRAL_DIR]));

  printf ("num_entries_centrl_dir_ths_disk = %d\n",
        makeword(&buffer[NUM_ENTRIES_CENTRL_DIR_THS_DISK]));
  printf ("total_entries_central_dir = %d\n",
        makeword(&buffer[TOTAL_ENTRIES_CENTRAL_DIR]));
  printf ("size_central_directory = %d\n",
        makelong(&buffer[SIZE_CENTRAL_DIRECTORY]));
  printf ("offset_start_central_directory = %d\n",
        makelong(&buffer[OFFSET_START_CENTRAL_DIRECTORY]));
  printf ("zipfile_comment_length = %d\n",
        makeword(&buffer[ZIPFILE_COMMENT_LENGTH]));
#endif

  dir_last_pad = 0;
  dir_ptr = zipf->central_directory;
  for (i = 0; i < zipf->count; i++)
    {
      ZipDirectory *zipd = (ZipDirectory*)(dir_ptr + dir_last_pad);
      long uncompressed_size = makelong (&dir_ptr[4+C_UNCOMPRESSED_SIZE]);
      long filename_length = makeword (&dir_ptr[4+C_FILENAME_LENGTH]);
      long extra_field_length = makeword (&dir_ptr[4+C_EXTRA_FIELD_LENGTH]);
      long file_comment_length = makeword (&dir_ptr[4+C_FILE_COMMENT_LENGTH]);
      int unpadded_direntry_length;
      if ((dir_ptr-zipf->central_directory)+filename_length+CREC_SIZE+4>zipf->dir_size)
	return -1;

      zipd->filename_length = filename_length;
      zipd->size = uncompressed_size;
#ifdef __GNUC__
#define DIR_ALIGN __alignof__(ZipDirectory)
#else
#define DIR_ALIGN sizeof(long)
#endif
      zipd->filestart = makelong (&dir_ptr[4+C_RELATIVE_OFFSET_LOCAL_HEADER])
	  + (LREC_SIZE+4) + filename_length + file_comment_length +
	  + (extra_field_length ? extra_field_length+4 : 0);
      /* About the last term of the expression above. Should the same
	 apply if file_comment_length is not zero ?  I've never seen
	 the comment field uses so far. FIXME.  */
      zipd->filename_offset = CREC_SIZE+4 - dir_last_pad;
      unpadded_direntry_length 
	  = zipd->filename_offset + zipd->filename_length + extra_field_length;
      zipd->direntry_size =
	((unpadded_direntry_length + DIR_ALIGN) / DIR_ALIGN) * DIR_ALIGN;
      dir_last_pad = zipd->direntry_size - unpadded_direntry_length;
      dir_ptr = (char*)zipd + unpadded_direntry_length;
      *dir_ptr = '\0';
    }
  return 0;
}

#ifdef TEST
main ()
{
  ZipFile zipf[1];
  ZipDirectory *zipd;
  int i;

  zipf->fd = 0;

  i = read_zip_archive (zipf);
  if (i)
    {
      fprintf (stderr, "Bad zip file.\n");
      exit (i);
    }

  zipd = (ZipDirectory*) zipf->central_directory;
  for (i = 0; i < zipf->count; i++, zipd = ZIPDIR_NEXT (zipd))
    {
      printf ("%d: size:%d, name(#%d)%s, offset:%d\n",
	      i, zipd->size, zipd->filename_length,
	      ZIPDIR_FILENAME (zipd),
	      zipd->filestart);
    }
}
#endif
