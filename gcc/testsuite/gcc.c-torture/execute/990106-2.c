unsigned calc_mp(unsigned mod)
{
      unsigned a,b,c;
      c=-1;
      a=c/mod;
      b=0-a*mod;
      if (b > mod) { a += 1; b-=mod; }
      return b;
}

int main(int argc, char *argv[])
{
      unsigned x = 1234;
      unsigned y = calc_mp(x);

      if (y != 680)
	abort ();
      exit (0);
}


