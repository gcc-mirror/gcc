// { dg-do compile }
// { dg-options "-O2 -fno-thread-jumps -fdump-tree-evrp" }

extern void link_error ();

void fast_sqrt (float);

float test (float x)
{
    float y = x*x;
    if (y >= 0.f)
      { 
        if (__builtin_isnan (y))
         link_error ();
        else
          fast_sqrt (y);

       if (!__builtin_isnan (y))
         fast_sqrt (y);
       else
         link_error ();
      }
}

// { dg-final { scan-tree-dump-times "fast_sqrt" 2 "evrp" } }
// { dg-final { scan-tree-dump-not "link_error" "evrp" } }
