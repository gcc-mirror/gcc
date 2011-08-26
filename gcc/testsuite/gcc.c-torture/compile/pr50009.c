/* PR target/50009 */

struct S {
  short a;
  short b[];
} __attribute__((packed));
