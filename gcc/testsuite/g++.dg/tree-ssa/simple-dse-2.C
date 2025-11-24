// { dg-do compile }
// { dg-options "-O2 -fno-exceptions -fdump-tree-forwprop1-details" }
// PR tree-optimization/122633
struct s1
{
  int f1[4];
};

struct s1 func1(int a);
void func2(int a)
{
  struct s1 v1 = func1(a);
}

__attribute__((pure))
struct s1 pure1(int a);
void func3(int a)
{
  struct s1 p1 = pure1(a);
}

// { dg-final { scan-tree-dump-not "Removing lhs of call stmt p1 =" "forwprop1" } }
// { dg-final { scan-tree-dump-not "Removing dead call store stmt v1 =" "forwprop1" } }
// { dg-final { scan-tree-dump "Removing lhs of call stmt v1 =" "forwprop1" } }
// { dg-final { scan-tree-dump "Removing dead call store stmt p1 =" "forwprop1" } }
// { dg-final { scan-tree-dump-times "Removing dead call store stmt" 1 "forwprop1" } }
// { dg-final { scan-tree-dump-times "Removing lhs of call stmt" 1 "forwprop1" } }

