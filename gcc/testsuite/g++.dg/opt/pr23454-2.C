/* PR rtl-optimization/23454 */
/* Submitted by Matthias Klose <doko@debian.org> */

/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef unsigned long long int ulonglong;
typedef long long int longlong;
typedef unsigned int uint32;
typedef unsigned int uint;
typedef unsigned long int ulong;

class Item {
public:
  bool null_value;
  virtual longlong val_int()=0;
};

typedef struct st_tree_element {
  struct st_tree_element *left,*right;
  uint32 count;
} TREE_ELEMENT;

typedef struct st_tree {
  uint offset_to_key,elements_in_tree,size_of_element,memory_limit,allocated;
  void *custom_arg;
  bool with_delete;
  uint flag;
} TREE;

class field_info
{
public:
  ulong treemem, tree_elements, empty, nulls, min_length, max_length;
  uint room_in_tree;
  bool found;
  TREE tree;
  Item *item;
};

class field_ulonglong: public field_info
{
  ulonglong min_arg, max_arg;
  ulonglong sum, sum_sqr;
  void add();
};

extern char *longlong10_to_str(longlong val,char *dst,int radix);
extern void delete_tree(TREE*);
extern TREE_ELEMENT *tree_insert(TREE *tree,void *custom_arg);

static int compare_ulonglong(const ulonglong *s, const ulonglong *t)
{
  return ((*s < *t) ? -1 : *s > *t ? 1 : 0);
}

void field_ulonglong::add()
{
  char buff[(255*3 +1)];
  longlong num = item->val_int();
  uint length = (uint) (longlong10_to_str(num, buff, 10) - buff);
  TREE_ELEMENT *element;

  if (item->null_value)
  {
    nulls++;
    return;
  }
  if (num == 0)
    empty++;

  if (room_in_tree)
  {
    if (!(element = tree_insert(&tree, tree.custom_arg)))
    {
      room_in_tree = 0;
      delete_tree(&tree);
    }
    else if (element->count == 1)
    {
      room_in_tree = 0;
      delete_tree(&tree);
    }
  }

  if (!found)
  {
    found = 1;
    min_arg = max_arg = sum = num;
    sum_sqr = num * num;
    min_length = max_length = length;
  }
  else if (num != 0)
  {
    sum += num;
    sum_sqr += num * num;
    if (length < min_length)
      min_length = length;
    if (length > max_length)
      max_length = length;
    if (compare_ulonglong((ulonglong*) &num, &min_arg) < 0)
      min_arg = num;
    if (compare_ulonglong((ulonglong*) &num, &max_arg) > 0)
      max_arg = num;
  }
}
