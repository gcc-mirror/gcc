// PR c/34506
// { dg-do compile }

#define p parallel

void
foo (int x)
{
#pragma omp p num_threads (4) if (1) private (x)
    ;
#pragma omp p num_threads(4)if(1)private(x)
    ;
#pragma omp p num_threads (4), if (1) , private (x)
    ;
#pragma omp p num_threads(4),if(1),private(x)
    ;
#pragma omp p, num_threads (4), if (1), private (x)
    ;
#pragma omp p num_threads (4), if (1), private (x),	// { dg-error "clause before" }
    ;
#pragma omp p num_threads (4), , if (1), private (x)	// { dg-error "clause before" }
    ;
}
