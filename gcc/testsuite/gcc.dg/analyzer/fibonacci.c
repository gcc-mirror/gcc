int fib (int n)
{
  if (n > 1)
    return fib (n - 1) + fib (n - 2);
  else
    return n;
} 

/* { dg-regexp "\[^\n\r\]+: warning: analysis bailed out early \\(\[0-9\]+ 'after-snode' enodes; \[0-9\]+ enodes\\) \[^\n\r\]*" } */
