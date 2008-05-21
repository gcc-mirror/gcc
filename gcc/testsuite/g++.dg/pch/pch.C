// { dg-options "-save-temps -fpch-preprocess -I." }

#include "pch.H"
int main() 
{
  return 0;
}

// { dg-final { cleanup-saved-temps ".s" } }
