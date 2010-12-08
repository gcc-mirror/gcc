// { dg-do assemble  }
// GROUPS passed operators
// opr-eq file
// Message-Id: <9301141514.AA05925@mi.el.utwente.nl>
// From: klamer@mi.el.utwente.nl (Klamer Schutte)
// Subject: 2.3.3: failed to detect error
// Date: Thu, 14 Jan 93 16:14:21 +0100

class B
{
public:
      operator=(B &); // { dg-error "no type" }
      // { dg-message "B::operator=|no known conversion" "note" { target *-*-* } 12 }
};

void
test(B &b1, const B &b2)
{
        b1 = b2;// { dg-error "match" }
	// { dg-message "candidate" "candidate note" { target *-*-* } 19 }
}
