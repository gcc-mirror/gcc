// { dg-do compile }
//
// PR 11553 catch duplicate friend specifiers

struct S
{
	friend friend class C; // { dg-error "duplicate" }
};


