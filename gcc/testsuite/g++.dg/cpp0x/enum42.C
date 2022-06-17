// PR c++/67048
// { dg-do compile { target c++11 } }
// { dg-options -Wpedantic }

typedef enum {} X;
enum {} x;
enum {} y, z;
