/* { dg-require-effective-target indirect_jumps } */

int fs_exec(int ino)
{
 void *src = 0;
 if (ino)
   src = (void*)0xe000;
 goto *src;
}
