/* { dg-lto-do link } */

#include "pr81440.h"

extern struct_t my_struct;

int main() {
 return my_struct.ints[0];
}
