// Build don't link:

// Test of severe performance regression from 2.95.  This code generates
// a heavily self-referential tree which caused the inliner to take
// O(3**N) time to scan it for function calls.
// Reported by Kelley Cook <kelley.cook@home.com>.  PR c++/1687.

bool in0 ;
bool in1 ;
bool in2 ;
bool in3 ;
bool in4 ;
bool in5 ;
bool in6 ;
bool in7 ;
bool in8 ;
bool in9 ;
bool in10;
bool in11;
bool in12;
bool in13;
bool in14;
bool in15;
bool in16;
bool in17;
bool in18;
bool in19;
bool in20;
bool in21;
bool in22;
bool in23;
bool in24;
bool in25;
bool in26;
bool in27;
bool in28;
bool in29;
bool in30;
bool in31;
unsigned long output;

void mux(void)
{
  output =
      (in0   ?  0x00000001 : 0) |
      (in1   ?  0x00000002 : 0) |
      (in2   ?  0x00000004 : 0) |
      (in3   ?  0x00000008 : 0) |
      (in4   ?  0x00000010 : 0) |
      (in5   ?  0x00000020 : 0) |
      (in6   ?  0x00000040 : 0) |
      (in7   ?  0x00000080 : 0) |
      (in8   ?  0x00000100 : 0) |
      (in9   ?  0x00000200 : 0) |
      (in10  ?  0x00000400 : 0) |
      (in11  ?  0x00000800 : 0) |
      (in12  ?  0x00001000 : 0) |
      (in13  ?  0x00002000 : 0) |
      (in14  ?  0x00004000 : 0) |
      (in15  ?  0x00008000 : 0) |
      (in16  ?  0x00010000 : 0) |
      (in17  ?  0x00020000 : 0) |
      (in18  ?  0x00040000 : 0) |
      (in19  ?  0x00080000 : 0) |
      (in20  ?  0x00100000 : 0) |
      (in21  ?  0x00200000 : 0) |
      (in22  ?  0x00400000 : 0) |
      (in23  ?  0x00800000 : 0) |
      (in24  ?  0x01000000 : 0) |
      (in25  ?  0x02000000 : 0) |
      (in26  ?  0x04000000 : 0) |
      (in27  ?  0x08000000 : 0) |
      (in28  ?  0x10000000 : 0) |
      (in29  ?  0x20000000 : 0) |
      (in30  ?  0x40000000 : 0) |
      (in31  ?  0x80000000 : 0) ;
}

