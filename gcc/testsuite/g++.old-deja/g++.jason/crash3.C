// { dg-do assemble  }
// Bug: g++ tries to generate initialization semantics for a Node from an int,
// and fails.

struct Node			// { dg-message "note" "" { target c++17_down } }
{
  Node* child[2];
};

void bug(int i)
{
  Node* q = new Node(i);	// { dg-error "no matching" "" { target c++17_down }  } 
// { dg-error "array must be initialized" "" { target c++2a } .-1 }
}
