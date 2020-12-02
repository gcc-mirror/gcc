/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch-optimized" } */

void fancy_abort(const char *, int, const char *);

enum machine_mode
{
  MODE_FLOAT,
  MODE_DECIMAL_FLOAT,
  MODE_COMPLEX_INT,
  MODE_COMPLEX_FLOAT,
  MODE_VECTOR_BOOL,
  MODE_VECTOR_FLOAT
} extern const mode_class;

void tree_node() {
  if (mode_class)
    mode_class == MODE_FLOAT || mode_class == MODE_DECIMAL_FLOAT ||
        mode_class == MODE_COMPLEX_FLOAT || mode_class == MODE_VECTOR_FLOAT
        ? fancy_abort("aaa", 2, __FUNCTION__),
        0 : 0;
  int g = 0;
}

/* { dg-final { scan-tree-dump "Condition chain with \[^\n\r]\* BBs transformed into a switch statement." "iftoswitch" } } */
