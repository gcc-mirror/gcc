extern int clobber_k (int *); 
extern int barrier (void); 
int t, u; 

int
process_second_stream(void) 
{ 
  int k; 
  int i = 0, j = 0, result; 
 
  clobber_k (&k); 
 
  while(t)
    ;
 
  while(!j) 
    {
      while(!j) 
	{ 
	  barrier (); 
	  if (t == 0) 
	    break; 
	  else if(t == 1) 
	    t = 2; 
	  else 
	    {
	      if(t < 0) 
		j=1; 
	      if(u < 0) 
		k = i++; 
	    }
	} 
 
      if(!j && u) 
	j=1; 
    } 
 
  return 0; 
} 
