/* PR c+/3816 */
/* { dg-do compile } */
namespace A {}
 
namespace OtherNamespace {
 
  typedef struct {
     int member;
   } A; // used to conflict with A namespace
 
} // end of namespace
 
