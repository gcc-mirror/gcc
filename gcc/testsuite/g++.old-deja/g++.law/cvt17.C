// Build don't link: 
// GROUPS passed conversions
// cvt file
// Message-Id: <9305210124.AA02409@kato.cs.brown.edu>
// From: pcm@cs.brown.edu (Peter C. McCluskey)
// Subject: illegal code compiles silently
// Date: Thu, 20 May 93 21:24:22 -0400



class Point {};
class Line_Segment{ public: Line_Segment(const Point&){} };
class Node { public: Point Location(){ Point p; return p; } };

int main()
{
   Node** node1;
   Line_Segment(node1->Location()); // intended (*node1)// ERROR - .*
}

