#include<map>
#include<iostream>
#include<vector>
#include<string>

using namespace std;

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
{  // ERROR - candidates
  public:
    // public type interface
    typedef std::map<int, EdgeType > Successor;
    typedef std::pair<VertexType, Successor> vertex;
    typedef std::vector<vertex> GraphType;
    typedef typename GraphType::iterator iterator;
    typedef typename GraphType::const_iterator const_iterator;

  // a lot of stuff deleted ....

  private:
    bool directed;
    GraphType C;          // container
    ostream* pOut;
};

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
            os << G[(*startN).first].first << ' ' // ERROR - no index operator
               << (*startN).second << ' ';
            ++startN;
        }
        os << ">\n";
    }
    return os;
}

int main()
{
    // no edge weighting, therefore type Empty:
    Graph<std::string, Empty> V(true);        // ERROR - no bool constructor
    // ReadGraph(V, "gra1.dat");

    // display of vertices with successors
    cout << V;

}
