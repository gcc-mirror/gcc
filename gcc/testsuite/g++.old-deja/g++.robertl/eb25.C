//Build don't link:
//Neither stack nor vector provide priority_queue, use <queue> instead
#include <stack>
#include <vector>


int main()
{
  priority_queue< int, vector<int>, greater<int> > pq; // ERROR - unknown template
  return 0;
}
