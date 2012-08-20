/* PR middle-end/53992 */
/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fopenmp" } */
/* { dg-require-effective-target fgnu_tm } */

int main() {
    long data[10000];
    long i, min=10000;
    for (i=0; i<10000; i++) data[i] = -i;
            
#pragma omp parallel for
    for (i=0; i<10000; i++) {
        __transaction_atomic
        {
            if (data[i] < min)
                min = data[i];
        }
    }

    return !(min == -9999);
}
