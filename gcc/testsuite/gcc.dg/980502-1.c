/* { dg-do compile  }*/
/* { dg-options "-O2" } */

char *const f(void)
{
        char *const line = "/dev/ptyXX";
        line[8] = 1;
        return line;
}
