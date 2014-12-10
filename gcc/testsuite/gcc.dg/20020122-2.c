/* This test was supplied with PR target/5379.  It caused the compiler
   to get a segmentation fault.  */
  
/* { dg-do compile } */
/* { dg-options "-O2 -fprefetch-loop-arrays -w" } */
/* { dg-additional-options "-march=athlon" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

extern int access( char* );
extern int strcmp(const char *s1, const char *s2);
extern int a();
char* foocp();

void
_KDE_IceGetPoValidAuthIndices (protocol_name, num_indices_ret, indices_ret)
const char *protocol_name;
int *num_indices_ret;
int *indices_ret;
{
    int auth_file;
    char *filename;
    int index_ret, i;

    if (!(filename = foocp ()))
        return;

    if (access (filename))
        return;

    if (auth_file = access (filename))
        return;

    for (;;)
    {
        if (strcmp (protocol_name, protocol_name) == 0 )
        {
            for (i = 0; i < *num_indices_ret; i++)
                if (index_ret == indices_ret[i])
                    break;
        }
    }
}

