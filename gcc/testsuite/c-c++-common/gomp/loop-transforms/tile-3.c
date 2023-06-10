extern void dummy (int);

void
test ()
{
    #pragma omp for
    #pragma omp tile sizes(1, 2) /* { dg-error {'tile' loop transformation may not appear on non-rectangular for} } */
    for (int i = 0; i < 100; ++i)
    for (int j = i; j < 100; ++j)
	dummy (i);

    #pragma omp for
    #pragma omp tile sizes(1, 2) /* { dg-error {'tile' loop transformation may not appear on non-rectangular for} } */
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < i; ++j)
	dummy (i);


#pragma omp for collapse(1)
    #pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
	dummy (i);

#pragma omp for collapse(2)
    #pragma omp tile sizes(1) /* { dg-error {nesting depth left after this transformation too low for loop collapse} } */
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
	dummy (i);

#pragma omp for collapse(2)
    #pragma omp tile sizes(1, 2)
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
	dummy (i);

#pragma omp for collapse(3)
    #pragma omp tile sizes(1, 2) /* { dg-error {nesting depth left after this transformation too low for loop collapse} } */
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} "" { target c } } */
    for (int j = 0; j < 100; ++j)
	dummy (i);
    /* { dg-error {not enough for loops to collapse} "" { target c++ } .-1 } */
    /* { dg-error {'i' was not declared in this scope} "" { target c++ } .-2 } */

#pragma omp for collapse(1)
#pragma omp tile sizes(1)
#pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
	dummy (i);

#pragma omp for collapse(2)
#pragma omp tile sizes(1, 2)
#pragma omp tile sizes(1) /* { dg-error {nesting depth left after this transformation too low for outer transformation} } */
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} "" { target c } } */
    	dummy (i);
    /* { dg-error {not enough for loops to collapse} "" { target c++ } .-1 } */

#pragma omp for collapse(2)
#pragma omp tile sizes(1, 2)
#pragma omp tile sizes(1, 2)
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} "" { target c } } */
    	dummy (i);
    /* { dg-error {not enough for loops to collapse} "" { target c++ } .-1 } */

#pragma omp for collapse(2)
#pragma omp tile sizes(5, 6)
#pragma omp tile sizes(1, 2, 3)
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} "" { target c } } */
    	dummy (i); 
    /* { dg-error {not enough for loops to collapse} "" { target c++ } .-1 } */

#pragma omp for collapse(1)
#pragma omp tile sizes(1)
#pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
	dummy (i);

#pragma omp for collapse(2)
#pragma omp tile sizes(1, 2)
#pragma omp tile sizes(1) /* { dg-error {nesting depth left after this transformation too low for outer transformation} } */
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    	dummy (i);

#pragma omp for collapse(2)
#pragma omp tile sizes(1, 2)
#pragma omp tile sizes(1, 2)
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    	dummy (i);

#pragma omp for collapse(2)
#pragma omp tile sizes(5, 6)
#pragma omp tile sizes(1, 2, 3)
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    for (int k = 0; k < 100; ++k)
    	dummy (i);

#pragma omp for collapse(3)
#pragma omp tile sizes(1, 2) /* { dg-error {nesting depth left after this transformation too low for loop collapse} } */
#pragma omp tile sizes(1, 2)
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} "" { target c } } */
    for (int j = 0; j < 100; ++j)
    	dummy (i);
    /* { dg-error {not enough for loops to collapse} "" { target c++ } .-1 } */

#pragma omp for collapse(3)
#pragma omp tile sizes(5, 6) /* { dg-error {nesting depth left after this transformation too low for loop collapse} } */
#pragma omp tile sizes(1, 2, 3)
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    for (int k = 0; k < 100; ++k)
    	dummy (i);
}
