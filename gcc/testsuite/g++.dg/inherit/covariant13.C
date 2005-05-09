// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 May 2005<nathan@codesourcery.com>

// Origin:Andrew Pinski: pinskia@gcc.gnu.org
// PR 21427: ICE on valid

struct B1 { 
  public: 
    virtual void foo(); 
}; 
 
struct B2 { 
  public: 
    virtual B2 & bar() = 0; 
}; 
 
struct I : public B1, B2 { 
  public: 
    virtual ~I(); 
    virtual I & bar(); 
}; 
 
struct D : public I { 
    virtual ~D(); 
}; 
