// { dg-do compile }
// { dg-options "-foptimize-sibling-calls -fcompare-debug -Wno-return-type" }

typedef union gimple_statement_d *gimple;
typedef gimple gimple_seq_node;
typedef struct {
    gimple_seq_node ptr;
    void *seq;
    void *bb;
} gimple_stmt_iterator;
struct gimple_statement_base {
    gimple next;
};
union gimple_statement_d {
    struct gimple_statement_base gsbase;
};
static inline gimple_stmt_iterator gsi_start_1 (gimple stmt)
{
  gimple_stmt_iterator i;
  i.ptr = stmt;
  return i;
}
bool gimple_may_fallthru (gimple);
static bool gimple_try_catch_may_fallthru (gimple stmt)
{
  gimple_stmt_iterator i = gsi_start_1 (stmt);
  for (; i.ptr; i.ptr = i.ptr->gsbase.next)
    {
      if (gimple_may_fallthru (i.ptr))
	return true;
    }
}
bool gimple_stmt_may_fallthru (gimple stmt, bool x)
{
  if (x)
    return gimple_may_fallthru (stmt);
  else
    return gimple_try_catch_may_fallthru (stmt);
}
