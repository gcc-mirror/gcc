// { dg-do assemble  }
// GROUPS passed references
const int& min(const int& n, const int& m)
{
        return n < m ? n : m;
}
