// Origin PR debug/
// { dg-options "-g -dA" }

class C {
public:
  C() {}
  ~C() {}
};
typedef struct {
  C m;
} t;
typedef t s;
s v;

/*
  We want to check that we have a DIE describing the typedef t like this:

	.uleb128 0xc	# (DIE (0xb8) DW_TAG_typedef)
	.ascii "t\0"	# DW_AT_name
	.byte	0x1	# DW_AT_decl_file (../../prtests/test.cc)
	.byte	0xb	# DW_AT_decl_line
	.long	0x78	# DW_AT_type

  e.g, it should not haven any child DIE -- the bug here was that this
  DIE had children DIEs. So we check that the last line is immediately
  followed by a line containing the pattern "(DIE (", instead of a
  line containing a DW_AT_sibling attribute.
 */

// { dg-final { scan-assembler-times "\[^\n\r\]*\\(DIE \[^\n\r\]* DW_TAG_typedef\\)\[\n\r\]{1,2}\[^\n\r\].*\"t\\\\0\"\[^\n\r\]*DW_AT_name\[\n\r\]{1,2}\[^\n\r\]*\[\n\r\]{1,2}\[^\n\r\]*\[\n\r\]{1,2}\[^\n\r\]*DW_AT_type\[\n\r\]{1,2}\[^\n\r\]*\\(DIE" 1 } }
