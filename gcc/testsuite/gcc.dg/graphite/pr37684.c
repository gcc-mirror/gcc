/* { dg-options "-O2 -fdump-tree-graphite-all" } */

typedef struct _IO_FILE FILE;
struct _IO_marker {
};
enum __codecvt_result
{
  __codecvt_noconv
};
struct _IO_FILE {
};
extern struct _IO_FILE *stderr;

typedef
   struct {
      unsigned int avail_in;
      unsigned int avail_out;
      void *state;
      void *(*bzalloc)(void *,int,int);
      void *opaque;
   }
   bz_stream;
extern int BZ2_bzCompressInit (
      bz_stream* strm,
      int blockSize100k,
      int verbosity,
      int workFactor
   );
typedef unsigned char Bool;
typedef int Int32;
typedef unsigned int UInt32;

typedef
   struct {
      Int32 mode;
      Int32 state;
      UInt32* arr1;
      UInt32* arr2;
      UInt32* ftab;
      Int32 nblock;
      Int32 nblockMAX;
      Bool inUse[256];
      Int32 blockNo;
   }
   EState;

void prepare_new_block ( EState* s )
{
   Int32 i;
   for (i = 0; i < 256; i++) s->inUse[i] = ((Bool)0);
   s->blockNo++;
}

int BZ2_bzCompressInit
                    ( bz_stream* strm,
                     int blockSize100k,
                     int verbosity,
                     int workFactor )
{
   EState* s;
   s = (strm->bzalloc)(strm->opaque,(sizeof(EState)),1);
   if (s->arr1 == ((void *)0) || s->arr2 == ((void *)0) || s->ftab == ((void *)0)) {
   }
   prepare_new_block ( s );
}

/* { dg-final { cleanup-tree-dump "graphite" } } */
