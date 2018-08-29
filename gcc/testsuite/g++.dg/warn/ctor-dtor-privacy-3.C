// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wctor-dtor-privacy" } 

class X // { dg-message "only defines private" }
{
public:
  X (X const &); // { dg-message "requires an existing" }
};

class Y // { dg-message "only defines private" }
{
public:
  Y (Y &&);  // { dg-message "requires an existing" }
};

class Z
{
public:
  Z (int);
};
