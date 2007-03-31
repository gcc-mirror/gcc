/* { dg-do compile } */
/* { dg-options "-O2" } */

class ios_base{};
struct basic_ostream : virtual ios_base{};
namespace
{
  struct Nullostream : basic_ostream{};
}
class  In
{
  In ();
  Nullostream  nullout;
};
In::In (){}
