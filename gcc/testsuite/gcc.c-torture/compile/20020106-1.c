/* Origin: PR c/5279 from <wilco@equator.com>.  */     
	
int
foo ()
{
  extern long long Y;
  return (0 > Y++);
}
