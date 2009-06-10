/* { dg-do compile } */
// middle-end/31579
// Expand was crashing while expanding the tree for the initializer

struct Industry {
 unsigned char produced_cargo[2];
};
unsigned int a = (((__SIZE_TYPE__)&reinterpret_cast<const volatile
char&>((((Industry*)(char*)8)->produced_cargo[0]))) - 8);

