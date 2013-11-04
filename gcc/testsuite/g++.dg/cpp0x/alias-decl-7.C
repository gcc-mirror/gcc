// { dg-options "-std=c++11" }

// Add arguments to unbound template template parameter.

template <template <class> class Template>
struct Internal {
  template <class Arg> using Bind = Template<Arg>;
};

template <template <class> class Template, class Arg>
using Instantiate = Template<Arg>; // After parsing #1, the
                                   // BOUND_TEMPLATE_TEMPLATE_PARM
                                   // parameter Template gets
                                   // the UNBOUND_CLASS_TEMPLATE
                                   // Internal<Template>::template Bind
                                   // as an argument, and the
                                   // parameter Arg gets Argument as
                                   // an argument.  And we build
                                   // 'Bind<Argument>'.

template <template <class> class Template, class Argument>
using Bind = Instantiate<Internal<Template>::template Bind, Argument>; //#1

