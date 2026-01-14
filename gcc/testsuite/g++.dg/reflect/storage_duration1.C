// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_{static,thread,automatic}_storage_duration.

#include <meta>

using namespace std::meta;

struct S {
  int nsdm;
  static_assert (!has_static_storage_duration (^^nsdm));
  static_assert (!has_thread_storage_duration (^^nsdm));
  static_assert (!has_automatic_storage_duration (^^nsdm));

  static inline int sdm;
  static_assert (has_static_storage_duration (^^sdm));
  static_assert (!has_thread_storage_duration (^^sdm));
  static_assert (!has_automatic_storage_duration (^^sdm));

  static thread_local int stdm;
  static_assert (!has_static_storage_duration (^^stdm));
  static_assert (has_thread_storage_duration (^^stdm));
  static_assert (!has_automatic_storage_duration (^^stdm));

  static void smemfn ();
  static_assert (!has_static_storage_duration (^^smemfn));
  static_assert (!has_thread_storage_duration (^^smemfn));
  static_assert (!has_automatic_storage_duration (^^smemfn));

  void memfn ();
  static_assert (!has_static_storage_duration (^^memfn));
  static_assert (!has_thread_storage_duration (^^memfn));
  static_assert (!has_automatic_storage_duration (^^memfn));
};

extern int i0;
static_assert (has_static_storage_duration (^^i0));
static_assert (!has_thread_storage_duration (^^i0));
static_assert (!has_automatic_storage_duration (^^i0));

int i1;
static_assert (has_static_storage_duration (^^i1));
static_assert (!has_thread_storage_duration (^^i1));
static_assert (!has_automatic_storage_duration (^^i1));

static int i2;
static_assert (has_static_storage_duration (^^i2));
static_assert (!has_thread_storage_duration (^^i2));
static_assert (!has_automatic_storage_duration (^^i2));

thread_local int i3;
static_assert (!has_static_storage_duration (^^i3));
static_assert (has_thread_storage_duration (^^i3));
static_assert (!has_automatic_storage_duration (^^i3));

static thread_local int i4;
static_assert (!has_static_storage_duration (^^i4));
static_assert (has_thread_storage_duration (^^i4));
static_assert (!has_automatic_storage_duration (^^i4));

void
foo (float parm)
{
  static_assert (!has_static_storage_duration (^^foo));
  static_assert (!has_thread_storage_duration (^^foo));
  static_assert (!has_automatic_storage_duration (^^foo));

  static_assert (!has_static_storage_duration (^^parm));
  static_assert (!has_thread_storage_duration (^^parm));
  static_assert (has_automatic_storage_duration (^^parm));

  int nonstatic_var;
  static_assert (!has_static_storage_duration (^^nonstatic_var));
  static_assert (!has_thread_storage_duration (^^nonstatic_var));
  static_assert (has_automatic_storage_duration (^^nonstatic_var));

  int& ref_to_nonstatic_var = nonstatic_var;
  static_assert (!has_static_storage_duration (^^ref_to_nonstatic_var));
  static_assert (!has_thread_storage_duration (^^ref_to_nonstatic_var));
  static_assert (has_automatic_storage_duration (^^ref_to_nonstatic_var));

  static int& static_ref_to_var = nonstatic_var;
  static_assert (has_static_storage_duration (^^static_ref_to_var));
  static_assert (!has_thread_storage_duration (^^static_ref_to_var));
  static_assert (!has_automatic_storage_duration (^^static_ref_to_var));

  static int static_var;
  static_assert (has_static_storage_duration (^^static_var));
  static_assert (!has_thread_storage_duration (^^static_var));
  static_assert (!has_automatic_storage_duration (^^static_var));

  int& ref_to_static_var = static_var;
  static_assert (!has_static_storage_duration (^^ref_to_static_var));
  static_assert (!has_thread_storage_duration (^^ref_to_static_var));
  static_assert (has_automatic_storage_duration (^^ref_to_static_var));

  thread_local int tl_var;
  static_assert (!has_static_storage_duration (^^tl_var));
  static_assert (has_thread_storage_duration (^^tl_var));
  static_assert (!has_automatic_storage_duration (^^tl_var));

  std::pair<int, int> p;
  auto [aa, ab] = p;
  static_assert (!has_static_storage_duration (^^aa));
  static_assert (!has_thread_storage_duration (^^aa));
  static_assert (!has_automatic_storage_duration (^^aa));

  static auto [sa, sb] = p;
  static_assert (!has_static_storage_duration (^^sa));
  static_assert (!has_thread_storage_duration (^^sa));
  static_assert (!has_automatic_storage_duration (^^sa));

  thread_local auto [ta, tb] = p;
  static_assert (!has_static_storage_duration (^^ta));
  static_assert (!has_thread_storage_duration (^^ta));
  static_assert (!has_automatic_storage_duration (^^ta));
}

template<auto V> struct TCls {};
static_assert (!has_static_storage_duration (template_arguments_of (^^TCls<5>)[0]));
static_assert (!has_thread_storage_duration (template_arguments_of (^^TCls<5>)[0]));
static_assert (!has_automatic_storage_duration (template_arguments_of (^^TCls<5>)[0]));

static_assert (has_static_storage_duration (template_arguments_of (^^TCls<S{}>)[0]));
static_assert (!has_thread_storage_duration (template_arguments_of (^^TCls<S{}>)[0]));
static_assert (!has_automatic_storage_duration (template_arguments_of (^^TCls<S{}>)[0]));

template<auto K> constexpr auto R = reflect_object (K);
static_assert (has_static_storage_duration (R<S{}>));
static_assert (!has_thread_storage_duration (R<S{}>));
static_assert (!has_automatic_storage_duration (R<S{}>));

static std::pair<int, int> p;
constexpr auto first = reflect_object (p.first);
static_assert (has_static_storage_duration (first));
static_assert (!has_thread_storage_duration (first));
static_assert (!has_automatic_storage_duration (first));

static_assert (!has_static_storage_duration (reflect_constant(4)));
static_assert (!has_thread_storage_duration (reflect_constant(4)));
static_assert (!has_automatic_storage_duration (reflect_constant(4)));
