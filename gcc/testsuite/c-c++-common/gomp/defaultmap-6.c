void f()
{
  struct s {
    int i;
  };
  int scalar1 = 5;
  int array1[5] = {1,2,3,4,5};
  int *ptr1 = &scalar1;
  struct s mystruct1 = {.i = 5};

  #pragma omp target defaultmap(firstprivate ) defaultmap(firstprivate : aggregate)  /* { dg-error "too many 'defaultmap' clauses with 'aggregate' category" }  */
   {
     scalar1 = 1; array1[0] = 2; if (ptr1 == 0L) mystruct1.i = 3;
   }

  #pragma omp target defaultmap(firstprivate : all ) defaultmap(alloc : pointer) /* { dg-error "too many 'defaultmap' clauses with 'pointer' category" }  */
   {
     scalar1 = 1; array1[0] = 2; if (ptr1 == 0L) mystruct1.i = 3;
   }


  #pragma omp target defaultmap(firstprivate : aggregate)  defaultmap(firstprivate ) /* { dg-error "too many 'defaultmap' clauses with 'aggregate' category" }  */
   {
     scalar1 = 1; array1[0] = 2; if (ptr1 == 0L) mystruct1.i = 3;
   }

  #pragma omp target defaultmap(alloc : pointer) defaultmap(firstprivate : all )  /* { dg-error "too many 'defaultmap' clauses with 'pointer' category" }  */
   {
     scalar1 = 1; array1[0] = 2; if (ptr1 == 0L) mystruct1.i = 3;
   }

  #pragma omp target defaultmap(firstprivate :all ) defaultmap(firstprivate : all) /* { dg-error "too many 'defaultmap' clauses with 'all' category" }  */
   {
     scalar1 = 1; array1[0] = 2; if (ptr1 == 0L) mystruct1.i = 3;
   }
  #pragma omp target defaultmap(firstprivate ) defaultmap(firstprivate) /* { dg-error "too many 'defaultmap' clauses with unspecified category" }  */
   {
     scalar1 = 1; array1[0] = 2; if (ptr1 == 0L) mystruct1.i = 3;
   }
  #pragma omp target defaultmap(firstprivate ) defaultmap(firstprivate : all) /* { dg-error "too many 'defaultmap' clauses with 'all' category" }  */
   {
     scalar1 = 1; array1[0] = 2; if (ptr1 == 0L) mystruct1.i = 3;
   }
  #pragma omp target defaultmap(firstprivate : all) defaultmap(firstprivate) /* { dg-error "too many 'defaultmap' clauses with 'all' category" }  */
   {
     scalar1 = 1; array1[0] = 2; if (ptr1 == 0L) mystruct1.i = 3;
   }
}
