/* PR tree-optimization/33961 */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-cselim" } */

void decode(char *d, int len);

void decode(char *d, int len) {
        int i = len - 1;
        while(i >= 0) {
                d[i];
                if(d[i] == 0)
                        d[i]=' ';
		if(d[i] == 1)
			d[i]='x';
                i--;
        }
}

int main(int argc, char **argv)
{
        decode("this bug is really weird", 24);
	return 0;
}
