// { dg-do assemble  }
// GROUPS passed initialization
class Tag {
public:
	Tag(int i):value(i){}
	int value;
};

extern const Tag myTag;
extern const Tag myTag=9;

// The compiler should not issue an error on this line; expand_static_init
// should be checking that there's no initializer for this line, and thus
// doesn't need to produce an error.
extern const Tag myTag;
