void abort (void);
void exit (int);

char a[] = "12345";
char b[] = "12345";

void loop (char * pz, char * pzDta)
{
    for (;;) {
        switch (*(pz++) = *(pzDta++)) {
        case 0:
  	    goto loopDone2;

	case '"':
	case '\\':
	    pz[-1]  = '\\';
            *(pz++) = pzDta[-1];
	}
    } loopDone2:;

  if (a - pz != b - pzDta)
    abort ();
}

int
main(void)
{
  loop (a, b);
  exit (0);
}
