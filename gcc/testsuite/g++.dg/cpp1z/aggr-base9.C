// PR c++/89214
// { dg-do compile { target c++17 } }

struct B {
  int c;
};

struct D : B { };

void
foo ()
{
  D d1 = {42};
  D d2 = {{42}};
  
  D d4 = D{42};
  D d5 = D{{42}};
 
  D d7 = {D{42}};
  D d8 = {D{{42}}};

  D d10 = {{D{42}}}; // { dg-warning "initializing a base class of type .B. results in object slicing" }
  D d11 = {{D{{42}}}}; // { dg-warning "initializing a base class of type .B. results in object slicing" }

  D dd1{42};
  D dd2{{42}};
  
  D dd7{D{42}};
  D dd8{D{{42}}};

  D dd10{{D{42}}}; // { dg-warning "initializing a base class of type .B. results in object slicing" }
  D dd11{{D{{42}}}}; // { dg-warning "initializing a base class of type .B. results in object slicing" }
}
