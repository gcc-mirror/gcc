/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-vars-details" } */

void link_error();


struct State {
    int p0, p1, p2;
    inline State(){p0=0;p1=0;p2=0;}
    inline State(const State &s) {
	p0 = s.p0;
	p1 = s.p1;
	p2 = s.p2;
    }
    
    inline void operator =(const State &s) {
	p0 = s.p0;
	p1 = s.p1;
	p2 = s.p2;
    }
    
    inline void step(void) {
	p0 = p1+p2;
	p1 = p0*p1+p2;
	p2 = p0-p2;
    }
};


inline void iterate_ok(State &inS1, State &inS2, unsigned int n)
{
    State s1 = inS1;
    for (unsigned int i = 0; i < n; i++) {
	s1.step();
    }
    inS1 = s1;
}

void temp()
{
  State s1;
  s1.p0 = 0;
  s1.p1 = 0;
  s1.p2 = 0;
  State s2;
  s2.p0 = 0;
  s2.p1 = 0;
  s2.p2 = 0;
  iterate_ok (s1, s2, 1);
  if (s1.p0)
   link_error();
  if (s1.p0)
   link_error();
  if (s1.p0)
   link_error();
}

/* We should have removed the casts from pointers to references and caused SRA to happen.  */

/* { dg-final { scan-tree-dump-times "link_error" 0 "vars"} } */
