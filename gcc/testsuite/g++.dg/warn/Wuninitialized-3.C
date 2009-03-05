// PR C++/38908
// { dg-options "-Wuninitialized -O" }

struct empty {};

struct dfs_visitor {
    dfs_visitor() { }
    empty m_vis;
};

void bar(const dfs_visitor&);
void foo(void)
{
  dfs_visitor vis;
  dfs_visitor vis2 = vis;
  bar (vis2);
}
