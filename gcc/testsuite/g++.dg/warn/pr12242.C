// PR 12242: should warn about out-of-range int->enum conversions
// { dg-do compile }
// { dg-options "-Wconversion -fpermissive" }
enum X { A };
enum Y { B, C, D };

void example ()
{
  int i = 5;
  X x;
  Y y;
  
  x = 10;  // { dg-warning "invalid conversion from .int. to .X." "invalid" }
           // { dg-warning "unspecified" "unspecified" { target *-*-* } 13 }
  x = 1;   // { dg-warning "invalid conversion from .int. to .X." }
  x = C;   // { dg-error "cannot convert .Y. to .X. in assignment" }  
  x = D;   // { dg-error "cannot convert .Y. to .X. in assignment" }  
  y = A;   // { dg-error "cannot convert .X. to .Y. in assignment" }  
  x = y;   // { dg-error "cannot convert .Y. to .X. in assignment" }  
  x = i;   // { dg-warning "invalid conversion from .int. to .X."  }
}

void foo () 
{
  X a = static_cast<X> (10); // { dg-warning "unspecified" }
  X b = static_cast<X> (0);
  X c = static_cast<X> (1);
  X d = static_cast<X> (2); // { dg-warning "unspecified" }
  X f = static_cast<X> ((int)A);
  X g = static_cast<X> (B);
  X h = static_cast<X> (C);
  X e = static_cast<X> (D); // { dg-warning "unspecified" }
}

enum QEvent { x = 42 }; 
 
int bar()
{ 
  QEvent x = ( QEvent ) 42000; // { dg-warning "unspecified" }
  return ( int ) x; 
}

enum W {a,b,c};
enum Z {d,e,f,g};
void bazz (int, int, int, int);

void baz() {
  int three = 3;
  int four = 4;
  bazz (
	W(three), 
	W(3), 
	Z(four), 
	Z(4) // { dg-warning "unspecified" }
	);
}

