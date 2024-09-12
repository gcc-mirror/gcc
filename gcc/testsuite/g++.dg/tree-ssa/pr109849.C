/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sra -fdump-tree-optimized" } */
/* { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } } */

#include <vector>
typedef unsigned int uint32_t;
std::pair<uint32_t, uint32_t> pair;
void
test()
{
        std::vector<std::pair<uint32_t, uint32_t> > stack;
        stack.push_back (pair);
        while (!stack.empty()) {
                std::pair<uint32_t, uint32_t> cur = stack.back();
                stack.pop_back();
                if (!cur.first)
                {
                        cur.second++;
                        stack.push_back (cur);
                }
                if (cur.second > 10000)
                        break;
        }
}
int
main()
{
        for (int i = 0; i < 10000; i++)
          test();
}

/* { dg-final { scan-tree-dump "Created a replacement for stack offset" "sra"} } */
/* { dg-final { scan-tree-dump-not "cur = MEM" "optimized"} } */
