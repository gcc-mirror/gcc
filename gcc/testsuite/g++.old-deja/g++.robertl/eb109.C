#include<map>
#include<iostream.h>
#include<vector>
#include<string>

// empty parameter class with a minimal set of operations
// if there are no weights for edges necessary
struct Empty
{
  public:
    Empty(int=0) {}
    bool operator<(const Empty&) const { return true;}
};
inline ostream& operator<<(ostream& os, const Empty&) { return os;}
inline istream& operator>>(istream& is, Empty& ) { return is;}


template<class VertexType, class EdgeType>
class Graph
{ 
  public:
    // public type interface
    typedef map<int, EdgeType > Successor;
    typedef pair<VertexType, Successor> vertex;
    typedef vector<vertex> GraphType;
    typedef typename GraphType::iterator iterator;
    typedef typename GraphType::const_iterator const_iterator;

  // a lot of stuff deleted ....

  private:
    bool directed;
    GraphType C;          // container
    ostream* pOut;
}; // ERROR - candidates

// all graph-methods delet
template<class VertexType, class EdgeType>
ostream& operator<<(ostream& os, Graph<VertexType,EdgeType>& G)
{
    // display of vertices with successors
  for(int i = 0; i < G.size(); ++i)  // ERROR - no size function
    {
      os << G[i].first << " <";      // ERROR - no index operator

        // The compiler does not like this line!!!!!!
        typename Graph<VertexType, EdgeType>::Successor::iterator
	  startN = G[i].second.begin(), // ERROR - no index operator
	  endN   = G[i].second.end();  // ERROR - no index operator

        while(startN != endN)
        {
            os << G[(*startN).first].first << ' ' // vertex
               << (*startN).second << ' ';        // ERROR - no index operator
            ++startN;
        }
        os << ">\n";
    }
    return os;
}

int main()
{
    // no edge weighting, therefore type Empty:
    Graph<string, Empty> V(true);        // ERROR - no bool constructor
    // ReadGraph(V, "gra1.dat");

    // display of vertices with successors
    cout << V;

}
