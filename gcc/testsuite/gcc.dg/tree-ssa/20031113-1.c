/* PR optimization/12640

   We used to get into an infinite loop while trying to
   figure out `strlen (resultString)'.  This showed up as
   a stack overflow while compiling tk.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */ 

extern __SIZE_TYPE__ strlen (const char *);

int i; 

static void 
SendEventProc (char *resultString) 
{ 
  char *p; 
 
  resultString = ""; 
  while (*p == '-') 
    { 
      if (p[2] == ' ') 
	{ 
	  resultString = p + 3; 
	} 
    } 
  for (;;) 
    { 
      i = strlen (resultString) + 1; 
    } 
}
 
