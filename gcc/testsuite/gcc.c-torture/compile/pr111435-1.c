/* { dg-options "-fexceptions -fno-early-inlining" } */
/* { dg-require-effective-target exceptions } */

void find_slot_with_hash(const int *);

void put(const int *k, const int *) {
    find_slot_with_hash(k);
}
unsigned len();
int *address();
void h(int header, int **bounds) {
  if (!*bounds)
    return;
  unsigned t = *bounds ? len() : 0;
  int queue_index = t;
  address()[(unsigned)queue_index] = 0;
  put(&header, &queue_index);
}
