/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -g" } */

typedef int __attribute__((vector_size(
    ((((((((((((((((((((((((((((((8 * sizeof(short)) * sizeof(short)) *
                                sizeof(short)) *
                               sizeof(short)) *
                              sizeof(short)) *
                             sizeof(short)) *
                            sizeof(short)) *
                           sizeof(short)) *
                          sizeof(short)) *
                         sizeof(short)) *
                        sizeof(short)) *
                       sizeof(short)) *
                      sizeof(short)) *
                     sizeof(short)) *
                    sizeof(short)) *
                   sizeof(short)) *
                  sizeof(short)) *
                 sizeof(short)) *
                sizeof(short)) *
               sizeof(short)) *
              sizeof(short)) *
             sizeof(short)) *
            sizeof(short)) *
           sizeof(short)) *
          sizeof(short)) *
         sizeof(short)) *
        sizeof(short)) *
       sizeof(short)) *
      sizeof(short)) *
     sizeof(short)) *
    sizeof(short)))) V; /* { dg-error "number of vector components" } */
void k() { V w = { 0 }; }
