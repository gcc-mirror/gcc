/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O" } */

typedef struct coordinate {
    double x;
} coordinate_t;

coordinate_t elementPtrC[3];

__attribute__((transaction_safe))
void TMelement_alloc (coordinate_t* coordinates, int numCoordinate)
{
   int i;
   for (i = 0; i < numCoordinate; i++) {
      elementPtrC[i] = coordinates[i];
   }
}
