/* { dg-do compile } */
/* { dg-options "-c -O2 -Wextra" } */
#define O_RDONLY     (1<<0)
#define O_WRONLY     (1<<1)
#define O_RDWR       (O_RDONLY|O_WRONLY)
#define O_CREAT      (1<<3)
#define O_TRUNC      (1<<6)

typedef enum {
    OM_READ = 0,
    OM_WRITE,
    OM_READWRITE_NOCREATE,
    OM_READWRITE_CREATE
} OpenMode;

extern int open(const char *name, int mode);

void open_file(const char *filename, const OpenMode rw)
{
    int mode = 0;

    switch( rw )
    {
    case OM_WRITE:
        mode = O_WRONLY|O_CREAT|O_TRUNC;
        break;
    case OM_READ:
        mode = O_RDONLY;
        break;
    case OM_READWRITE_NOCREATE:
        mode = O_RDWR;
        break;
    case OM_READWRITE_CREATE:
        mode = O_RDWR|O_CREAT|O_TRUNC;
        break;
    }

    open( filename, mode );
}
