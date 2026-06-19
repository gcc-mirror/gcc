#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct Base {};
struct Derived : Base {};
struct Unrelated {};

struct Child : Base {};
struct Grandchild : Child {};

struct SiblingA : Base {};
struct SiblingB : Base {};

struct B1 {};
struct B2 {};
struct MultiDerived : B1, B2 {};

struct Amb {};
struct Mid1 : Amb {};
struct Mid2 : Amb {};
struct AmbDerived : Mid1, Mid2 {};

struct PrivDerived : private Base {};

void test_unrelated ()
{
  try {
    throw Derived ();
  }
  catch (Unrelated &) {
    __analyzer_dump_path (); // { dg-bogus "path" }
  }
}

void test_object_vs_pointer ()
{
  try {
    throw Derived ();
  }
  catch (Base *) {
    __analyzer_dump_path (); // { dg-bogus "path" }
  }
}

void test_wrong_direction ()
{
  try {
    throw Base ();
  }
  catch (Derived &) {
    __analyzer_dump_path (); // { dg-bogus "path" }
  }
}

void test_pointer_base ()
{
  static Derived d;
  try {
    throw &d;
  }
  catch (Base *) {
    __analyzer_dump_path (); // { dg-message "path" }
  }
}

void test_grandchild ()
{
  try {
    throw Grandchild ();
  }
  catch (Base &) {
    __analyzer_dump_path (); // { dg-message "path" }
  }
}

void test_sibling ()
{
  try {
    throw SiblingA ();
  }
  catch (SiblingB &) {
    __analyzer_dump_path (); // { dg-bogus "path" }
  }
}

void test_multiple_inheritance ()
{
  try {
    throw MultiDerived ();
  }
  catch (B1 &) {
    __analyzer_dump_path (); // { dg-message "path" }
  }
}

void test_multiple_inheritance_b2 ()
{
  try {
    throw MultiDerived ();
  }
  catch (B2 &) {
    __analyzer_dump_path (); // { dg-message "path" }
  }
}

void test_ambiguous_base ()
{
  try {
    throw AmbDerived ();
  }
  catch (Amb &) {
    __analyzer_dump_path (); // { dg-bogus "path" }
  }
}

void test_private_base ()
{
  try {
    throw PrivDerived ();
  }
  catch (Base &) {
    __analyzer_dump_path (); // { dg-bogus "path" }
  }
}

void test_cv_qualified ()
{
  try {
    throw Derived ();
  }
  catch (const Base &) {
    __analyzer_dump_path (); // { dg-message "path" }
  }
}
