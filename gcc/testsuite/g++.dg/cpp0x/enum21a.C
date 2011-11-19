// { dg-do compile }
// { dg-options "-pedantic -std=c++98" }

enum x { y, }; // { dg-warning "comma at end of enumerator list" }
