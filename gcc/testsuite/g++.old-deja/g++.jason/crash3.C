// Bug: g++ tries to generate initialization semantics for a Node from an int,
// and fails.
// Build don't link:

struct Node
{
  Node* child[2];
};				// ERROR - 

void bug(int i)
{
  Node* q = new Node(i);	// ERROR - 
}
