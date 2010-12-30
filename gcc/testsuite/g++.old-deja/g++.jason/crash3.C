// { dg-do assemble  }
// Bug: g++ tries to generate initialization semantics for a Node from an int,
// and fails.

struct Node			// { dg-message "note" }
{
  Node* child[2];
};

void bug(int i)
{
  Node* q = new Node(i);	// { dg-error "no matching" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 12 }
}
