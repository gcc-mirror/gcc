// { dg-do assemble { xfail arm-*-pe } }
// { dg-options "-fexceptions" }

int atoi(const char *);

struct ios {
 virtual ~ios();
};
 
class fstreambase : virtual public ios {
};

class ifstream : public fstreambase {
};

class ofstream : public fstreambase {
};

extern const short O;
extern const short D;

const short O=  0;
const short D= -3;


short glc(const char* const * const l,
	  short& n,short& x,short& y,
	  ifstream* i,ofstream* o)

{
  n=atoi(l[1]);

  x=atoi(l[2]);
  y=atoi(l[3]);

  if((x < 0)||(y <0))
    {
     return D;
    }

  i = new ifstream[n];
  o = new ofstream[2];

  return O;

}
