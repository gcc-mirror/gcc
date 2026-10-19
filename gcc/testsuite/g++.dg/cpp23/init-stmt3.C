// PR c++/127780
// { dg-do compile { target c++23 } }

void
f (int x)
{
  if (using T = int; : x) {}	    // { dg-error "expected primary-expression before .:.|expected .\\)." }
  switch (using T = int; : x) {}    // { dg-error "expected primary-expression before .:.|expected .\\)." }
  for (using T = int; : x) {}	    // { dg-error "expected primary-expression before .:.|expected .\\)." }
}

// { dg-prune-output {before .\}. token} }
