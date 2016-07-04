.. |with| replace:: *with*
.. |withs| replace:: *with*\ s
.. |withed| replace:: *with*\ ed
.. |withing| replace:: *with*\ ing

.. -- Example: A |withing| unit has a |with| clause, it |withs| a |withed| unit


.. _Elaboration_Order_Handling_in_GNAT:

**********************************
Elaboration Order Handling in GNAT
**********************************

.. index:: Order of elaboration
.. index:: Elaboration control

This appendix describes the handling of elaboration code in Ada and
in GNAT, and discusses how the order of elaboration of program units can
be controlled in GNAT, either automatically or with explicit programming
features.

.. _Elaboration_Code:

Elaboration Code
================

Ada provides rather general mechanisms for executing code at elaboration
time, that is to say before the main program starts executing. Such code arises
in three contexts:

* *Initializers for variables*

  Variables declared at the library level, in package specs or bodies, can
  require initialization that is performed at elaboration time, as in:

  .. code-block:: ada

       Sqrt_Half : Float := Sqrt (0.5);

* *Package initialization code*

  Code in a `BEGIN-END` section at the outer level of a package body is
  executed as part of the package body elaboration code.

* *Library level task allocators*

  Tasks that are declared using task allocators at the library level
  start executing immediately and hence can execute at elaboration time.

Subprogram calls are possible in any of these contexts, which means that
any arbitrary part of the program may be executed as part of the elaboration
code. It is even possible to write a program which does all its work at
elaboration time, with a null main program, although stylistically this
would usually be considered an inappropriate way to structure
a program.

An important concern arises in the context of elaboration code:
we have to be sure that it is executed in an appropriate order. What we
have is a series of elaboration code sections, potentially one section
for each unit in the program. It is important that these execute
in the correct order. Correctness here means that, taking the above
example of the declaration of `Sqrt_Half`,
if some other piece of
elaboration code references `Sqrt_Half`,
then it must run after the
section of elaboration code that contains the declaration of
`Sqrt_Half`.

There would never be any order of elaboration problem if we made a rule
that whenever you |with| a unit, you must elaborate both the spec and body
of that unit before elaborating the unit doing the |withing|:

.. code-block:: ada

     with Unit_1;
     package Unit_2 is ...

would require that both the body and spec of `Unit_1` be elaborated
before the spec of `Unit_2`. However, a rule like that would be far too
restrictive. In particular, it would make it impossible to have routines
in separate packages that were mutually recursive.

You might think that a clever enough compiler could look at the actual
elaboration code and determine an appropriate correct order of elaboration,
but in the general case, this is not possible. Consider the following
example.

In the body of `Unit_1`, we have a procedure `Func_1`
that references
the variable `Sqrt_1`, which is declared in the elaboration code
of the body of `Unit_1`:

.. code-block:: ada

     Sqrt_1 : Float := Sqrt (0.1);

The elaboration code of the body of `Unit_1` also contains:

.. code-block:: ada

     if expression_1 = 1 then
        Q := Unit_2.Func_2;
     end if;

`Unit_2` is exactly parallel,
it has a procedure `Func_2` that references
the variable `Sqrt_2`, which is declared in the elaboration code of
the body `Unit_2`:

.. code-block:: ada

      Sqrt_2 : Float := Sqrt (0.1);

The elaboration code of the body of `Unit_2` also contains:

.. code-block:: ada

     if expression_2 = 2 then
        Q := Unit_1.Func_1;
     end if;

Now the question is, which of the following orders of elaboration is
acceptable:

::

     Spec of Unit_1
     Spec of Unit_2
     Body of Unit_1
     Body of Unit_2

or

::

     Spec of Unit_2
     Spec of Unit_1
     Body of Unit_2
     Body of Unit_1

If you carefully analyze the flow here, you will see that you cannot tell
at compile time the answer to this question.
If `expression_1` is not equal to 1,
and `expression_2` is not equal to 2,
then either order is acceptable, because neither of the function calls is
executed. If both tests evaluate to true, then neither order is acceptable
and in fact there is no correct order.

If one of the two expressions is true, and the other is false, then one
of the above orders is correct, and the other is incorrect. For example,
if `expression_1` /= 1 and `expression_2` = 2,
then the call to `Func_1`
will occur, but not the call to `Func_2.`
This means that it is essential
to elaborate the body of `Unit_1` before
the body of `Unit_2`, so the first
order of elaboration is correct and the second is wrong.

By making `expression_1` and `expression_2`
depend on input data, or perhaps
the time of day, we can make it impossible for the compiler or binder
to figure out which of these expressions will be true, and hence it
is impossible to guarantee a safe order of elaboration at run time.

.. _Checking_the_Elaboration_Order:

Checking the Elaboration Order
==============================

In some languages that involve the same kind of elaboration problems,
e.g., Java and C++, the programmer needs to take these
ordering problems into account, and it is common to
write a program in which an incorrect elaboration order  gives
surprising results, because it references variables before they
are initialized.
Ada is designed to be a safe language, and a programmer-beware approach is
clearly not sufficient. Consequently, the language provides three lines
of defense:

* *Standard rules*

  Some standard rules restrict the possible choice of elaboration
  order. In particular, if you |with| a unit, then its spec is always
  elaborated before the unit doing the |with|. Similarly, a parent
  spec is always elaborated before the child spec, and finally
  a spec is always elaborated before its corresponding body.

.. index:: Elaboration checks
.. index:: Checks, elaboration

* *Dynamic elaboration checks*

  Dynamic checks are made at run time, so that if some entity is accessed
  before it is elaborated (typically  by means of a subprogram call)
  then the exception (`Program_Error`) is raised.

* *Elaboration control*

  Facilities are provided for the programmer to specify the desired order
  of elaboration.

Let's look at these facilities in more detail. First, the rules for
dynamic checking. One possible rule would be simply to say that the
exception is raised if you access a variable which has not yet been
elaborated. The trouble with this approach is that it could require
expensive checks on every variable reference. Instead Ada has two
rules which are a little more restrictive, but easier to check, and
easier to state:

* *Restrictions on calls*

  A subprogram can only be called at elaboration time if its body
  has been elaborated. The rules for elaboration given above guarantee
  that the spec of the subprogram has been elaborated before the
  call, but not the body. If this rule is violated, then the
  exception `Program_Error` is raised.

* *Restrictions on instantiations*

  A generic unit can only be instantiated if the body of the generic
  unit has been elaborated. Again, the rules for elaboration given above
  guarantee that the spec of the generic unit has been elaborated
  before the instantiation, but not the body. If this rule is
  violated, then the exception `Program_Error` is raised.

The idea is that if the body has been elaborated, then any variables
it references must have been elaborated; by checking for the body being
elaborated we guarantee that none of its references causes any
trouble. As we noted above, this is a little too restrictive, because a
subprogram that has no non-local references in its body may in fact be safe
to call. However, it really would be unsafe to rely on this, because
it would mean that the caller was aware of details of the implementation
in the body. This goes against the basic tenets of Ada.

A plausible implementation can be described as follows.
A Boolean variable is associated with each subprogram
and each generic unit. This variable is initialized to False, and is set to
True at the point body is elaborated. Every call or instantiation checks the
variable, and raises `Program_Error` if the variable is False.

Note that one might think that it would be good enough to have one Boolean
variable for each package, but that would not deal with cases of trying
to call a body in the same package as the call
that has not been elaborated yet.
Of course a compiler may be able to do enough analysis to optimize away
some of the Boolean variables as unnecessary, and `GNAT` indeed
does such optimizations, but still the easiest conceptual model is to
think of there being one variable per subprogram.

.. _Controlling_the_Elaboration_Order:

Controlling the Elaboration Order
=================================

In the previous section we discussed the rules in Ada which ensure
that `Program_Error` is raised if an incorrect elaboration order is
chosen. This prevents erroneous executions, but we need mechanisms to
specify a correct execution and avoid the exception altogether.
To achieve this, Ada provides a number of features for controlling
the order of elaboration. We discuss these features in this section.

First, there are several ways of indicating to the compiler that a given
unit has no elaboration problems:

* *packages that do not require a body*

  A library package that does not require a body does not permit
  a body (this rule was introduced in Ada 95).
  Thus if we have a such a package, as in:

  .. code-block:: ada

       package Definitions is
          generic
             type m is new integer;
          package Subp is
             type a is array (1 .. 10) of m;
             type b is array (1 .. 20) of m;
          end Subp;
       end Definitions;

  A package that |withs| `Definitions` may safely instantiate
  `Definitions.Subp` because the compiler can determine that there
  definitely is no package body to worry about in this case

.. index:: pragma Pure

* *pragma Pure*

  This pragma places sufficient restrictions on a unit to guarantee that
  no call to any subprogram in the unit can result in an
  elaboration problem. This means that the compiler does not need
  to worry about the point of elaboration of such units, and in
  particular, does not need to check any calls to any subprograms
  in this unit.

.. index:: pragma Preelaborate

* *pragma Preelaborate*

  This pragma places slightly less stringent restrictions on a unit than
  does pragma Pure,
  but these restrictions are still sufficient to ensure that there
  are no elaboration problems with any calls to the unit.

.. index:: pragma Elaborate_Body

* *pragma Elaborate_Body*

  This pragma requires that the body of a unit be elaborated immediately
  after its spec. Suppose a unit `A` has such a pragma,
  and unit `B` does
  a |with| of unit `A`. Recall that the standard rules require
  the spec of unit `A`
  to be elaborated before the |withing| unit; given the pragma in
  `A`, we also know that the body of `A`
  will be elaborated before `B`, so
  that calls to `A` are safe and do not need a check.

  Note that, unlike pragma `Pure` and pragma `Preelaborate`,
  the use of `Elaborate_Body` does not guarantee that the program is
  free of elaboration problems, because it may not be possible
  to satisfy the requested elaboration order.
  Let's go back to the example with `Unit_1` and `Unit_2`.
  If a programmer marks `Unit_1` as `Elaborate_Body`,
  and not `Unit_2,` then the order of
  elaboration will be::

       Spec of Unit_2
       Spec of Unit_1
       Body of Unit_1
       Body of Unit_2

  Now that means that the call to `Func_1` in `Unit_2`
  need not be checked,
  it must be safe. But the call to `Func_2` in
  `Unit_1` may still fail if
  `Expression_1` is equal to 1,
  and the programmer must still take
  responsibility for this not being the case.

  If all units carry a pragma `Elaborate_Body`, then all problems are
  eliminated, except for calls entirely within a body, which are
  in any case fully under programmer control. However, using the pragma
  everywhere is not always possible.
  In particular, for our `Unit_1`/`Unit_2` example, if
  we marked both of them as having pragma `Elaborate_Body`, then
  clearly there would be no possible elaboration order.

The above pragmas allow a server to guarantee safe use by clients, and
clearly this is the preferable approach. Consequently a good rule
is to mark units as `Pure` or `Preelaborate` if possible,
and if this is not possible,
mark them as `Elaborate_Body` if possible.
As we have seen, there are situations where neither of these
three pragmas can be used.
So we also provide methods for clients to control the
order of elaboration of the servers on which they depend:

.. index:: pragma Elaborate

* *pragma Elaborate (unit)*

  This pragma is placed in the context clause, after a |with| clause,
  and it requires that the body of the named unit be elaborated before
  the unit in which the pragma occurs. The idea is to use this pragma
  if the current unit calls at elaboration time, directly or indirectly,
  some subprogram in the named unit.


.. index:: pragma Elaborate_All

* *pragma Elaborate_All (unit)*

  This is a stronger version of the Elaborate pragma. Consider the
  following example::

        Unit A |withs| unit B and calls B.Func in elab code
        Unit B |withs| unit C, and B.Func calls C.Func


  Now if we put a pragma `Elaborate (B)`
  in unit `A`, this ensures that the
  body of `B` is elaborated before the call, but not the
  body of `C`, so
  the call to `C.Func` could still cause `Program_Error` to
  be raised.

  The effect of a pragma `Elaborate_All` is stronger, it requires
  not only that the body of the named unit be elaborated before the
  unit doing the |with|, but also the bodies of all units that the
  named unit uses, following |with| links transitively. For example,
  if we put a pragma `Elaborate_All (B)` in unit `A`,
  then it requires not only that the body of `B` be elaborated before `A`,
  but also the body of `C`, because `B` |withs| `C`.

We are now in a position to give a usage rule in Ada for avoiding
elaboration problems, at least if dynamic dispatching and access to
subprogram values are not used. We will handle these cases separately
later.

The rule is simple:

*If a unit has elaboration code that can directly or
indirectly make a call to a subprogram in a |withed| unit, or instantiate
a generic package in a |withed| unit,
then if the |withed| unit does not have
pragma `Pure` or `Preelaborate`, then the client should have
a pragma `Elaborate_All`for the |withed| unit.**

By following this rule a client is
assured that calls can be made without risk of an exception.

For generic subprogram instantiations, the rule can be relaxed to
require only a pragma `Elaborate` since elaborating the body
of a subprogram cannot cause any transitive elaboration (we are
not calling the subprogram in this case, just elaborating its
declaration).

If this rule is not followed, then a program may be in one of four
states:

* *No order exists*

  No order of elaboration exists which follows the rules, taking into
  account any `Elaborate`, `Elaborate_All`,
  or `Elaborate_Body` pragmas. In
  this case, an Ada compiler must diagnose the situation at bind
  time, and refuse to build an executable program.

* *One or more orders exist, all incorrect*

  One or more acceptable elaboration orders exist, and all of them
  generate an elaboration order problem. In this case, the binder
  can build an executable program, but `Program_Error` will be raised
  when the program is run.

* *Several orders exist, some right, some incorrect*

  One or more acceptable elaboration orders exists, and some of them
  work, and some do not. The programmer has not controlled
  the order of elaboration, so the binder may or may not pick one of
  the correct orders, and the program may or may not raise an
  exception when it is run. This is the worst case, because it means
  that the program may fail when moved to another compiler, or even
  another version of the same compiler.

* *One or more orders exists, all correct*

  One ore more acceptable elaboration orders exist, and all of them
  work. In this case the program runs successfully. This state of
  affairs can be guaranteed by following the rule we gave above, but
  may be true even if the rule is not followed.

Note that one additional advantage of following our rules on the use
of `Elaborate` and `Elaborate_All`
is that the program continues to stay in the ideal (all orders OK) state
even if maintenance
changes some bodies of some units. Conversely, if a program that does
not follow this rule happens to be safe at some point, this state of affairs
may deteriorate silently as a result of maintenance changes.

You may have noticed that the above discussion did not mention
the use of `Elaborate_Body`. This was a deliberate omission. If you
|with| an `Elaborate_Body` unit, it still may be the case that
code in the body makes calls to some other unit, so it is still necessary
to use `Elaborate_All` on such units.


.. _Controlling_Elaboration_in_GNAT_-_Internal_Calls:

Controlling Elaboration in GNAT - Internal Calls
================================================

In the case of internal calls, i.e., calls within a single package, the
programmer has full control over the order of elaboration, and it is up
to the programmer to elaborate declarations in an appropriate order. For
example writing:

.. code-block:: ada

     function One return Float;

     Q : Float := One;

     function One return Float is
     begin
          return 1.0;
     end One;

will obviously raise `Program_Error` at run time, because function
One will be called before its body is elaborated. In this case GNAT will
generate a warning that the call will raise `Program_Error`::

     1. procedure y is
     2.    function One return Float;
     3.
     4.    Q : Float := One;
                        |
        >>> warning: cannot call "One" before body is elaborated
        >>> warning: Program_Error will be raised at run time

     5.
     6.    function One return Float is
     7.    begin
     8.         return 1.0;
     9.    end One;
    10.
    11. begin
    12.    null;
    13. end;


Note that in this particular case, it is likely that the call is safe, because
the function `One` does not access any global variables.
Nevertheless in Ada, we do not want the validity of the check to depend on
the contents of the body (think about the separate compilation case), so this
is still wrong, as we discussed in the previous sections.

The error is easily corrected by rearranging the declarations so that the
body of `One` appears before the declaration containing the call
(note that in Ada 95 as well as later versions of the Ada standard,
declarations can appear in any order, so there is no restriction that
would prevent this reordering, and if we write:

.. code-block:: ada

     function One return Float;

     function One return Float is
     begin
          return 1.0;
     end One;

     Q : Float := One;

then all is well, no warning is generated, and no
`Program_Error` exception
will be raised.
Things are more complicated when a chain of subprograms is executed:

.. code-block:: ada

     function A return Integer;
     function B return Integer;
     function C return Integer;

     function B return Integer is begin return A; end;
     function C return Integer is begin return B; end;

     X : Integer := C;

     function A return Integer is begin return 1; end;

Now the call to `C`
at elaboration time in the declaration of `X` is correct, because
the body of `C` is already elaborated,
and the call to `B` within the body of
`C` is correct, but the call
to `A` within the body of `B` is incorrect, because the body
of `A` has not been elaborated, so `Program_Error`
will be raised on the call to `A`.
In this case GNAT will generate a
warning that `Program_Error` may be
raised at the point of the call. Let's look at the warning::

     1. procedure x is
     2.    function A return Integer;
     3.    function B return Integer;
     4.    function C return Integer;
     5.
     6.    function B return Integer is begin return A; end;
                                                        |
        >>> warning: call to "A" before body is elaborated may
                     raise Program_Error
        >>> warning: "B" called at line 7
        >>> warning: "C" called at line 9

     7.    function C return Integer is begin return B; end;
     8.
     9.    X : Integer := C;
    10.
    11.    function A return Integer is begin return 1; end;
    12.
    13. begin
    14.    null;
    15. end;


Note that the message here says 'may raise', instead of the direct case,
where the message says 'will be raised'. That's because whether
`A` is
actually called depends in general on run-time flow of control.
For example, if the body of `B` said

.. code-block:: ada

     function B return Integer is
     begin
        if some-condition-depending-on-input-data then
           return A;
        else
           return 1;
        end if;
     end B;

then we could not know until run time whether the incorrect call to A would
actually occur, so `Program_Error` might
or might not be raised. It is possible for a compiler to
do a better job of analyzing bodies, to
determine whether or not `Program_Error`
might be raised, but it certainly
couldn't do a perfect job (that would require solving the halting problem
and is provably impossible), and because this is a warning anyway, it does
not seem worth the effort to do the analysis. Cases in which it
would be relevant are rare.

In practice, warnings of either of the forms given
above will usually correspond to
real errors, and should be examined carefully and eliminated.
In the rare case where a warning is bogus, it can be suppressed by any of
the following methods:

* Compile with the *-gnatws* switch set

* Suppress `Elaboration_Check` for the called subprogram

* Use pragma `Warnings_Off` to turn warnings off for the call

For the internal elaboration check case,
GNAT by default generates the
necessary run-time checks to ensure
that `Program_Error` is raised if any
call fails an elaboration check. Of course this can only happen if a
warning has been issued as described above. The use of pragma
`Suppress (Elaboration_Check)` may (but is not guaranteed to) suppress
some of these checks, meaning that it may be possible (but is not
guaranteed) for a program to be able to call a subprogram whose body
is not yet elaborated, without raising a `Program_Error` exception.


.. _Controlling_Elaboration_in_GNAT_-_External_Calls:

Controlling Elaboration in GNAT - External Calls
================================================

The previous section discussed the case in which the execution of a
particular thread of elaboration code occurred entirely within a
single unit. This is the easy case to handle, because a programmer
has direct and total control over the order of elaboration, and
furthermore, checks need only be generated in cases which are rare
and which the compiler can easily detect.
The situation is more complex when separate compilation is taken into account.
Consider the following:

.. code-block:: ada

      package Math is
         function Sqrt (Arg : Float) return Float;
      end Math;

      package body Math is
         function Sqrt (Arg : Float) return Float is
         begin
               ...
         end Sqrt;
      end Math;

      with Math;
      package Stuff is
         X : Float := Math.Sqrt (0.5);
      end Stuff;

      with Stuff;
      procedure Main is
      begin
         ...
      end Main;

where `Main` is the main program. When this program is executed, the
elaboration code must first be executed, and one of the jobs of the
binder is to determine the order in which the units of a program are
to be elaborated. In this case we have four units: the spec and body
of `Math`,
the spec of `Stuff` and the body of `Main`).
In what order should the four separate sections of elaboration code
be executed?

There are some restrictions in the order of elaboration that the binder
can choose. In particular, if unit U has a |with|
for a package `X`, then you
are assured that the spec of `X`
is elaborated before U , but you are
not assured that the body of `X`
is elaborated before U.
This means that in the above case, the binder is allowed to choose the
order::

     spec of Math
     spec of Stuff
     body of Math
     body of Main

but that's not good, because now the call to `Math.Sqrt`
that happens during
the elaboration of the `Stuff`
spec happens before the body of `Math.Sqrt` is
elaborated, and hence causes `Program_Error` exception to be raised.
At first glance, one might say that the binder is misbehaving, because
obviously you want to elaborate the body of something you |with| first, but
that is not a general rule that can be followed in all cases. Consider

.. code-block:: ada

      package X is ...

      package Y is ...

      with X;
      package body Y is ...

      with Y;
      package body X is ...

This is a common arrangement, and, apart from the order of elaboration
problems that might arise in connection with elaboration code, this works fine.
A rule that says that you must first elaborate the body of anything you
|with| cannot work in this case:
the body of `X` |withs| `Y`,
which means you would have to
elaborate the body of `Y` first, but that |withs| `X`,
which means
you have to elaborate the body of `X` first, but ... and we have a
loop that cannot be broken.

It is true that the binder can in many cases guess an order of elaboration
that is unlikely to cause a `Program_Error`
exception to be raised, and it tries to do so (in the
above example of `Math/Stuff/Spec`, the GNAT binder will
by default
elaborate the body of `Math` right after its spec, so all will be well).

However, a program that blindly relies on the binder to be helpful can
get into trouble, as we discussed in the previous sections, so GNAT
provides a number of facilities for assisting the programmer in
developing programs that are robust with respect to elaboration order.


.. _Default_Behavior_in_GNAT_-_Ensuring_Safety:

Default Behavior in GNAT - Ensuring Safety
==========================================

The default behavior in GNAT ensures elaboration safety. In its
default mode GNAT implements the
rule we previously described as the right approach. Let's restate it:

*If a unit has elaboration code that can directly or indirectly make a
call to a subprogram in a |withed| unit, or instantiate a generic
package in a |withed| unit, then if the |withed| unit
does not have pragma `Pure` or `Preelaborate`, then the client should have an
`Elaborate_All` pragma for the |withed| unit.*

*In the case of instantiating a generic subprogram, it is always
sufficient to have only an `Elaborate` pragma for the
|withed| unit.*

By following this rule a client is assured that calls and instantiations
can be made without risk of an exception.

In this mode GNAT traces all calls that are potentially made from
elaboration code, and puts in any missing implicit `Elaborate`
and `Elaborate_All` pragmas.
The advantage of this approach is that no elaboration problems
are possible if the binder can find an elaboration order that is
consistent with these implicit `Elaborate` and
`Elaborate_All` pragmas. The
disadvantage of this approach is that no such order may exist.

If the binder does not generate any diagnostics, then it means that it has
found an elaboration order that is guaranteed to be safe. However, the binder
may still be relying on implicitly generated `Elaborate` and
`Elaborate_All` pragmas so portability to other compilers than GNAT is not
guaranteed.

If it is important to guarantee portability, then the compilations should
use the *-gnatel*
(info messages for elaboration pragmas) switch. This will cause info messages
to be generated indicating the missing `Elaborate` and
`Elaborate_All` pragmas.
Consider the following source program:

.. code-block:: ada

     with k;
     package j is
       m : integer := k.r;
     end;

where it is clear that there
should be a pragma `Elaborate_All`
for unit `k`. An implicit pragma will be generated, and it is
likely that the binder will be able to honor it. However, if you want
to port this program to some other Ada compiler than GNAT.
it is safer to include the pragma explicitly in the source. If this
unit is compiled with the *-gnatel*
switch, then the compiler outputs an information message::

     1. with k;
     2. package j is
     3.   m : integer := k.r;
                          |
        >>> info: call to "r" may raise Program_Error
        >>> info: missing pragma Elaborate_All for "k"

     4. end;

and these messages can be used as a guide for supplying manually
the missing pragmas. It is usually a bad idea to use this
option during development. That's because it will tell you when
you need to put in a pragma, but cannot tell you when it is time
to take it out. So the use of pragma `Elaborate_All` may lead to
unnecessary dependencies and even false circularities.

This default mode is more restrictive than the Ada Reference
Manual, and it is possible to construct programs which will compile
using the dynamic model described there, but will run into a
circularity using the safer static model we have described.

Of course any Ada compiler must be able to operate in a mode
consistent with the requirements of the Ada Reference Manual,
and in particular must have the capability of implementing the
standard dynamic model of elaboration with run-time checks.

In GNAT, this standard mode can be achieved either by the use of
the *-gnatE* switch on the compiler (*gcc* or
*gnatmake*) command, or by the use of the configuration pragma:

.. code-block:: ada

      pragma Elaboration_Checks (DYNAMIC);

Either approach will cause the unit affected to be compiled using the
standard dynamic run-time elaboration checks described in the Ada
Reference Manual. The static model is generally preferable, since it
is clearly safer to rely on compile and link time checks rather than
run-time checks. However, in the case of legacy code, it may be
difficult to meet the requirements of the static model. This
issue is further discussed in
:ref:`What_to_Do_If_the_Default_Elaboration_Behavior_Fails`.

Note that the static model provides a strict subset of the allowed
behavior and programs of the Ada Reference Manual, so if you do
adhere to the static model and no circularities exist,
then you are assured that your program will
work using the dynamic model, providing that you remove any
pragma Elaborate statements from the source.


.. _Treatment_of_Pragma_Elaborate:

Treatment of Pragma Elaborate
=============================

.. index:: Pragma Elaborate

The use of `pragma Elaborate`
should generally be avoided in Ada 95 and Ada 2005 programs,
since there is no guarantee that transitive calls
will be properly handled. Indeed at one point, this pragma was placed
in Annex J (Obsolescent Features), on the grounds that it is never useful.

Now that's a bit restrictive. In practice, the case in which
`pragma Elaborate` is useful is when the caller knows that there
are no transitive calls, or that the called unit contains all necessary
transitive `pragma Elaborate` statements, and legacy code often
contains such uses.

Strictly speaking the static mode in GNAT should ignore such pragmas,
since there is no assurance at compile time that the necessary safety
conditions are met. In practice, this would cause GNAT to be incompatible
with correctly written Ada 83 code that had all necessary
`pragma Elaborate` statements in place. Consequently, we made the
decision that GNAT in its default mode will believe that if it encounters
a `pragma Elaborate` then the programmer knows what they are doing,
and it will trust that no elaboration errors can occur.

The result of this decision is two-fold. First to be safe using the
static mode, you should remove all `pragma Elaborate` statements.
Second, when fixing circularities in existing code, you can selectively
use `pragma Elaborate` statements to convince the static mode of
GNAT that it need not generate an implicit `pragma Elaborate_All`
statement.

When using the static mode with *-gnatwl*, any use of
`pragma Elaborate` will generate a warning about possible
problems.


.. _Elaboration_Issues_for_Library_Tasks:

Elaboration Issues for Library Tasks
====================================

.. index:: Library tasks, elaboration issues

.. index:: Elaboration of library tasks

In this section we examine special elaboration issues that arise for
programs that declare library level tasks.

Generally the model of execution of an Ada program is that all units are
elaborated, and then execution of the program starts. However, the
declaration of library tasks definitely does not fit this model. The
reason for this is that library tasks start as soon as they are declared
(more precisely, as soon as the statement part of the enclosing package
body is reached), that is to say before elaboration
of the program is complete. This means that if such a task calls a
subprogram, or an entry in another task, the callee may or may not be
elaborated yet, and in the standard
Reference Manual model of dynamic elaboration checks, you can even
get timing dependent Program_Error exceptions, since there can be
a race between the elaboration code and the task code.

The static model of elaboration in GNAT seeks to avoid all such
dynamic behavior, by being conservative, and the conservative
approach in this particular case is to assume that all the code
in a task body is potentially executed at elaboration time if
a task is declared at the library level.

This can definitely result in unexpected circularities. Consider
the following example

.. code-block:: ada

      package Decls is
        task Lib_Task is
           entry Start;
        end Lib_Task;

        type My_Int is new Integer;

        function Ident (M : My_Int) return My_Int;
      end Decls;

      with Utils;
      package body Decls is
        task body Lib_Task is
        begin
           accept Start;
           Utils.Put_Val (2);
        end Lib_Task;

        function Ident (M : My_Int) return My_Int is
        begin
           return M;
        end Ident;
      end Decls;

      with Decls;
      package Utils is
        procedure Put_Val (Arg : Decls.My_Int);
      end Utils;

      with Text_IO;
      package body Utils is
        procedure Put_Val (Arg : Decls.My_Int) is
        begin
           Text_IO.Put_Line (Decls.My_Int'Image (Decls.Ident (Arg)));
        end Put_Val;
      end Utils;

      with Decls;
      procedure Main is
      begin
         Decls.Lib_Task.Start;
      end;

If the above example is compiled in the default static elaboration
mode, then a circularity occurs. The circularity comes from the call
`Utils.Put_Val` in the task body of `Decls.Lib_Task`. Since
this call occurs in elaboration code, we need an implicit pragma
`Elaborate_All` for `Utils`. This means that not only must
the spec and body of `Utils` be elaborated before the body
of `Decls`, but also the spec and body of any unit that is
|withed| by the body of `Utils` must also be elaborated before
the body of `Decls`. This is the transitive implication of
pragma `Elaborate_All` and it makes sense, because in general
the body of `Put_Val` might have a call to something in a
|withed| unit.

In this case, the body of Utils (actually its spec) |withs|
`Decls`. Unfortunately this means that the body of `Decls`
must be elaborated before itself, in case there is a call from the
body of `Utils`.

Here is the exact chain of events we are worrying about:

* In the body of `Decls` a call is made from within the body of a library
  task to a subprogram in the package `Utils`. Since this call may
  occur at elaboration time (given that the task is activated at elaboration
  time), we have to assume the worst, i.e., that the
  call does happen at elaboration time.

* This means that the body and spec of `Util` must be elaborated before
  the body of `Decls` so that this call does not cause an access before
  elaboration.

* Within the body of `Util`, specifically within the body of
  `Util.Put_Val` there may be calls to any unit |withed|
  by this package.

* One such |withed| package is package `Decls`, so there
  might be a call to a subprogram in `Decls` in `Put_Val`.
  In fact there is such a call in this example, but we would have to
  assume that there was such a call even if it were not there, since
  we are not supposed to write the body of `Decls` knowing what
  is in the body of `Utils`; certainly in the case of the
  static elaboration model, the compiler does not know what is in
  other bodies and must assume the worst.

* This means that the spec and body of `Decls` must also be
  elaborated before we elaborate the unit containing the call, but
  that unit is `Decls`! This means that the body of `Decls`
  must be elaborated before itself, and that's a circularity.

Indeed, if you add an explicit pragma `Elaborate_All` for `Utils` in
the body of `Decls` you will get a true Ada Reference Manual
circularity that makes the program illegal.

In practice, we have found that problems with the static model of
elaboration in existing code often arise from library tasks, so
we must address this particular situation.

Note that if we compile and run the program above, using the dynamic model of
elaboration (that is to say use the *-gnatE* switch),
then it compiles, binds,
links, and runs, printing the expected result of 2. Therefore in some sense
the circularity here is only apparent, and we need to capture
the properties of this program that  distinguish it from other library-level
tasks that have real elaboration problems.

We have four possible answers to this question:


* Use the dynamic model of elaboration.

  If we use the *-gnatE* switch, then as noted above, the program works.
  Why is this? If we examine the task body, it is apparent that the task cannot
  proceed past the
  `accept` statement until after elaboration has been completed, because
  the corresponding entry call comes from the main program, not earlier.
  This is why the dynamic model works here. But that's really giving
  up on a precise analysis, and we prefer to take this approach only if we cannot
  solve the
  problem in any other manner. So let us examine two ways to reorganize
  the program to avoid the potential elaboration problem.

* Split library tasks into separate packages.

  Write separate packages, so that library tasks are isolated from
  other declarations as much as possible. Let us look at a variation on
  the above program.


  .. code-block:: ada

      package Decls1 is
        task Lib_Task is
           entry Start;
        end Lib_Task;
      end Decls1;

      with Utils;
      package body Decls1 is
        task body Lib_Task is
        begin
           accept Start;
           Utils.Put_Val (2);
        end Lib_Task;
      end Decls1;

      package Decls2 is
        type My_Int is new Integer;
        function Ident (M : My_Int) return My_Int;
      end Decls2;

      with Utils;
      package body Decls2 is
        function Ident (M : My_Int) return My_Int is
        begin
           return M;
        end Ident;
      end Decls2;

      with Decls2;
      package Utils is
        procedure Put_Val (Arg : Decls2.My_Int);
      end Utils;

      with Text_IO;
      package body Utils is
        procedure Put_Val (Arg : Decls2.My_Int) is
        begin
           Text_IO.Put_Line (Decls2.My_Int'Image (Decls2.Ident (Arg)));
        end Put_Val;
      end Utils;

      with Decls1;
      procedure Main is
      begin
         Decls1.Lib_Task.Start;
      end;


  All we have done is to split `Decls` into two packages, one
  containing the library task, and one containing everything else. Now
  there is no cycle, and the program compiles, binds, links and executes
  using the default static model of elaboration.

* Declare separate task types.

  A significant part of the problem arises because of the use of the
  single task declaration form. This means that the elaboration of
  the task type, and the elaboration of the task itself (i.e., the
  creation of the task) happen at the same time. A good rule
  of style in Ada is to always create explicit task types. By
  following the additional step of placing task objects in separate
  packages from the task type declaration, many elaboration problems
  are avoided. Here is another modified example of the example program:

  .. code-block:: ada

      package Decls is
        task type Lib_Task_Type is
           entry Start;
        end Lib_Task_Type;

        type My_Int is new Integer;

        function Ident (M : My_Int) return My_Int;
      end Decls;

      with Utils;
      package body Decls is
        task body Lib_Task_Type is
        begin
           accept Start;
           Utils.Put_Val (2);
        end Lib_Task_Type;

        function Ident (M : My_Int) return My_Int is
        begin
           return M;
        end Ident;
      end Decls;

      with Decls;
      package Utils is
        procedure Put_Val (Arg : Decls.My_Int);
      end Utils;

      with Text_IO;
      package body Utils is
        procedure Put_Val (Arg : Decls.My_Int) is
        begin
           Text_IO.Put_Line (Decls.My_Int'Image (Decls.Ident (Arg)));
        end Put_Val;
      end Utils;

      with Decls;
      package Declst is
         Lib_Task : Decls.Lib_Task_Type;
      end Declst;

      with Declst;
      procedure Main is
      begin
         Declst.Lib_Task.Start;
      end;


  What we have done here is to replace the `task` declaration in
  package `Decls` with a `task type` declaration. Then we
  introduce a separate package `Declst` to contain the actual
  task object. This separates the elaboration issues for
  the `task type`
  declaration, which causes no trouble, from the elaboration issues
  of the task object, which is also unproblematic, since it is now independent
  of the elaboration of  `Utils`.
  This separation of concerns also corresponds to
  a generally sound engineering principle of separating declarations
  from instances. This version of the program also compiles, binds, links,
  and executes, generating the expected output.

.. index:: No_Entry_Calls_In_Elaboration_Code restriction

* Use No_Entry_Calls_In_Elaboration_Code restriction.

  The previous two approaches described how a program can be restructured
  to avoid the special problems caused by library task bodies. in practice,
  however, such restructuring may be difficult to apply to existing legacy code,
  so we must consider solutions that do not require massive rewriting.

  Let us consider more carefully why our original sample program works
  under the dynamic model of elaboration. The reason is that the code
  in the task body blocks immediately on the `accept`
  statement. Now of course there is nothing to prohibit elaboration
  code from making entry calls (for example from another library level task),
  so we cannot tell in isolation that
  the task will not execute the accept statement  during elaboration.

  However, in practice it is very unusual to see elaboration code
  make any entry calls, and the pattern of tasks starting
  at elaboration time and then immediately blocking on `accept` or
  `select` statements is very common. What this means is that
  the compiler is being too pessimistic when it analyzes the
  whole package body as though it might be executed at elaboration
  time.

  If we know that the elaboration code contains no entry calls, (a very safe
  assumption most of the time, that could almost be made the default
  behavior), then we can compile all units of the program under control
  of the following configuration pragma:

  .. code-block:: ada

      pragma Restrictions (No_Entry_Calls_In_Elaboration_Code);

  This pragma can be placed in the :file:`gnat.adc` file in the usual
  manner. If we take our original unmodified program and compile it
  in the presence of a :file:`gnat.adc` containing the above pragma,
  then once again, we can compile, bind, link, and execute, obtaining
  the expected result. In the presence of this pragma, the compiler does
  not trace calls in a task body, that appear after the first `accept`
  or `select` statement, and therefore does not report a potential
  circularity in the original program.

  The compiler will check to the extent it can that the above
  restriction is not violated, but it is not always possible to do a
  complete check at compile time, so it is important to use this
  pragma only if the stated restriction is in fact met, that is to say
  no task receives an entry call before elaboration of all units is completed.


.. _Mixing_Elaboration_Models:

Mixing Elaboration Models
=========================

So far, we have assumed that the entire program is either compiled
using the dynamic model or static model, ensuring consistency. It
is possible to mix the two models, but rules have to be followed
if this mixing is done to ensure that elaboration checks are not
omitted.

The basic rule is that
**a unit compiled with the static model cannot
be |withed| by a unit compiled with the dynamic model**.
The reason for this is that in the static model, a unit assumes that
its clients guarantee to use (the equivalent of) pragma
`Elaborate_All` so that no elaboration checks are required
in inner subprograms, and this assumption is violated if the
client is compiled with dynamic checks.

The precise rule is as follows. A unit that is compiled with dynamic
checks can only |with| a unit that meets at least one of the
following criteria:


* The |withed| unit is itself compiled with dynamic elaboration
  checks (that is with the *-gnatE* switch.

* The |withed| unit is an internal GNAT implementation unit from
  the System, Interfaces, Ada, or GNAT hierarchies.

* The |withed| unit has pragma Preelaborate or pragma Pure.

* The |withing| unit (that is the client) has an explicit pragma
  `Elaborate_All` for the |withed| unit.


If this rule is violated, that is if a unit with dynamic elaboration
checks |withs| a unit that does not meet one of the above four
criteria, then the binder (`gnatbind`) will issue a warning
similar to that in the following example::

     warning: "x.ads" has dynamic elaboration checks and with's
     warning:   "y.ads" which has static elaboration checks

These warnings indicate that the rule has been violated, and that as a result
elaboration checks may be missed in the resulting executable file.
This warning may be suppressed using the *-ws* binder switch
in the usual manner.

One useful application of this mixing rule is in the case of a subsystem
which does not itself |with| units from the remainder of the
application. In this case, the entire subsystem can be compiled with
dynamic checks to resolve a circularity in the subsystem, while
allowing the main application that uses this subsystem to be compiled
using the more reliable default static model.


.. _What_to_Do_If_the_Default_Elaboration_Behavior_Fails:

What to Do If the Default Elaboration Behavior Fails
====================================================

If the binder cannot find an acceptable order, it outputs detailed
diagnostics. For example::

     error: elaboration circularity detected
     info:   "proc (body)" must be elaborated before "pack (body)"
     info:     reason: Elaborate_All probably needed in unit "pack (body)"
     info:     recompile "pack (body)" with -gnatel
     info:                             for full details
     info:       "proc (body)"
     info:         is needed by its spec:
     info:       "proc (spec)"
     info:         which is withed by:
     info:       "pack (body)"
     info:  "pack (body)" must be elaborated before "proc (body)"
     info:     reason: pragma Elaborate in unit "proc (body)"

In this case we have a cycle that the binder cannot break. On the one
hand, there is an explicit pragma Elaborate in `proc` for
`pack`. This means that the body of `pack` must be elaborated
before the body of `proc`. On the other hand, there is elaboration
code in `pack` that calls a subprogram in `proc`. This means
that for maximum safety, there should really be a pragma
Elaborate_All in `pack` for `proc` which would require that
the body of `proc` be elaborated before the body of
`pack`. Clearly both requirements cannot be satisfied.
Faced with a circularity of this kind, you have three different options.


* *Fix the program*

  The most desirable option from the point of view of long-term maintenance
  is to rearrange the program so that the elaboration problems are avoided.
  One useful technique is to place the elaboration code into separate
  child packages. Another is to move some of the initialization code to
  explicitly called subprograms, where the program controls the order
  of initialization explicitly. Although this is the most desirable option,
  it may be impractical and involve too much modification, especially in
  the case of complex legacy code.

* *Perform dynamic checks*

  If the compilations are done using the *-gnatE*
  (dynamic elaboration check) switch, then GNAT behaves in a quite different
  manner. Dynamic checks are generated for all calls that could possibly result
  in raising an exception. With this switch, the compiler does not generate
  implicit `Elaborate` or `Elaborate_All` pragmas. The behavior then is
  exactly as specified in the :title:`Ada Reference Manual`.
  The binder will generate
  an executable program that may or may not raise `Program_Error`, and then
  it is the programmer's job to ensure that it does not raise an exception. Note
  that it is important to compile all units with the switch, it cannot be used
  selectively.

* *Suppress checks*

  The drawback of dynamic checks is that they generate a
  significant overhead at run time, both in space and time. If you
  are absolutely sure that your program cannot raise any elaboration
  exceptions, and you still want to use the dynamic elaboration model,
  then you can use the configuration pragma
  `Suppress (Elaboration_Check)` to suppress all such checks. For
  example this pragma could be placed in the :file:`gnat.adc` file.

* *Suppress checks selectively*

  When you know that certain calls or instantiations in elaboration code cannot
  possibly lead to an elaboration error, and the binder nevertheless complains
  about implicit `Elaborate` and `Elaborate_All` pragmas that lead to
  elaboration circularities, it is possible to remove those warnings locally and
  obtain a program that will bind. Clearly this can be unsafe, and it is the
  responsibility of the programmer to make sure that the resulting program has no
  elaboration anomalies. The pragma `Suppress (Elaboration_Check)` can be
  used with different granularity to suppress warnings and break elaboration
  circularities:

  * Place the pragma that names the called subprogram in the declarative part
    that contains the call.

  * Place the pragma in the declarative part, without naming an entity. This
    disables warnings on all calls in the corresponding  declarative region.

  * Place the pragma in the package spec that declares the called subprogram,
    and name the subprogram. This disables warnings on all elaboration calls to
    that subprogram.

  * Place the pragma in the package spec that declares the called subprogram,
    without naming any entity. This disables warnings on all elaboration calls to
    all subprograms declared in this spec.

  * Use Pragma Elaborate.

    As previously described in section :ref:`Treatment_of_Pragma_Elaborate`,
    GNAT in static mode assumes that a `pragma` Elaborate indicates correctly
    that no elaboration checks are required on calls to the designated unit.
    There may be cases in which the caller knows that no transitive calls
    can occur, so that a `pragma Elaborate` will be sufficient in a
    case where `pragma Elaborate_All` would cause a circularity.

  These five cases are listed in order of decreasing safety, and therefore
  require increasing programmer care in their application. Consider the
  following program:

  .. code-block:: ada

        package Pack1 is
          function F1 return Integer;
          X1 : Integer;
        end Pack1;

        package Pack2 is
          function F2 return Integer;
          function Pure (x : integer) return integer;
          --  pragma Suppress (Elaboration_Check, On => Pure);  -- (3)
          --  pragma Suppress (Elaboration_Check);              -- (4)
        end Pack2;

        with Pack2;
        package body Pack1 is
          function F1 return Integer is
          begin
            return 100;
          end F1;
          Val : integer := Pack2.Pure (11);    --  Elab. call (1)
        begin
          declare
            --  pragma Suppress(Elaboration_Check, Pack2.F2);   -- (1)
            --  pragma Suppress(Elaboration_Check);             -- (2)
          begin
            X1 := Pack2.F2 + 1;                --  Elab. call (2)
          end;
        end Pack1;

        with Pack1;
        package body Pack2 is
          function F2 return Integer is
          begin
             return Pack1.F1;
          end F2;
          function Pure (x : integer) return integer is
          begin
             return x ** 3 - 3 * x;
          end;
        end Pack2;

        with Pack1, Ada.Text_IO;
        procedure Proc3 is
        begin
          Ada.Text_IO.Put_Line(Pack1.X1'Img); -- 101
        end Proc3;

  In the absence of any pragmas, an attempt to bind this program produces
  the following diagnostics::

       error: elaboration circularity detected
       info:    "pack1 (body)" must be elaborated before "pack1 (body)"
       info:       reason: Elaborate_All probably needed in unit "pack1 (body)"
       info:       recompile "pack1 (body)" with -gnatel for full details
       info:          "pack1 (body)"
       info:             must be elaborated along with its spec:
       info:          "pack1 (spec)"
       info:             which is withed by:
       info:          "pack2 (body)"
       info:             which must be elaborated along with its spec:
       info:          "pack2 (spec)"
       info:             which is withed by:
       info:          "pack1 (body)"

  The sources of the circularity are the two calls to `Pack2.Pure` and
  `Pack2.F2` in the body of `Pack1`. We can see that the call to
  F2 is safe, even though F2 calls F1, because the call appears after the
  elaboration of the body of F1. Therefore the pragma (1) is safe, and will
  remove the warning on the call. It is also possible to use pragma (2)
  because there are no other potentially unsafe calls in the block.

  The call to `Pure` is safe because this function does not depend on the
  state of `Pack2`. Therefore any call to this function is safe, and it
  is correct to place pragma (3) in the corresponding package spec.

  Finally, we could place pragma (4) in the spec of `Pack2` to disable
  warnings on all calls to functions declared therein. Note that this is not
  necessarily safe, and requires more detailed examination of the subprogram
  bodies involved. In particular, a call to `F2` requires that `F1`
  be already elaborated.

It is hard to generalize on which of these four approaches should be
taken. Obviously if it is possible to fix the program so that the default
treatment works, this is preferable, but this may not always be practical.
It is certainly simple enough to use *-gnatE*
but the danger in this case is that, even if the GNAT binder
finds a correct elaboration order, it may not always do so,
and certainly a binder from another Ada compiler might not. A
combination of testing and analysis (for which the
information messages generated with the *-gnatel*
switch can be useful) must be used to ensure that the program is free
of errors. One switch that is useful in this testing is the
*-p (pessimistic elaboration order)* switch for `gnatbind`.
Normally the binder tries to find an order that has the best chance
of avoiding elaboration problems. However, if this switch is used, the binder
plays a devil's advocate role, and tries to choose the order that
has the best chance of failing. If your program works even with this
switch, then it has a better chance of being error free, but this is still
not a guarantee.

For an example of this approach in action, consider the C-tests (executable
tests) from the ACATS suite. If these are compiled and run with the default
treatment, then all but one of them succeed without generating any error
diagnostics from the binder. However, there is one test that fails, and
this is not surprising, because the whole point of this test is to ensure
that the compiler can handle cases where it is impossible to determine
a correct order statically, and it checks that an exception is indeed
raised at run time.

This one test must be compiled and run using the *-gnatE*
switch, and then it passes. Alternatively, the entire suite can
be run using this switch. It is never wrong to run with the dynamic
elaboration switch if your code is correct, and we assume that the
C-tests are indeed correct (it is less efficient, but efficiency is
not a factor in running the ACATS tests.)


.. _Elaboration_for_Indirect_Calls:

Elaboration for Indirect Calls
==============================

.. index:: Dispatching calls
.. index:: Indirect calls

In rare cases, the static elaboration model fails to prevent
dispatching calls to not-yet-elaborated subprograms. In such cases, we
fall back to run-time checks; premature calls to any primitive
operation of a tagged type before the body of the operation has been
elaborated will raise `Program_Error`.

Access-to-subprogram types, however, are handled conservatively in many
cases. This was not true in earlier versions of the compiler; you can use
the *-gnatd.U* debug switch to revert to the old behavior if the new
conservative behavior causes elaboration cycles. Here, 'conservative' means
that if you do `P'Access` during elaboration, the compiler will normally
assume that you might call `P` indirectly during elaboration, so it adds an
implicit `pragma Elaborate_All` on the library unit containing `P`. The
*-gnatd.U* switch is safe if you know there are no such calls. If the
program worked before, it will continue to work with *-gnatd.U*. But beware
that code modifications such as adding an indirect call can cause erroneous
behavior in the presence of *-gnatd.U*.

These implicit Elaborate_All pragmas are not added in all cases, because
they cause elaboration cycles in certain common code patterns. If you want
even more conservative handling of P'Access, you can use the *-gnatd.o*
switch.

See `debug.adb` for documentation on the *-gnatd...* debug switches.


.. _Summary_of_Procedures_for_Elaboration_Control:

Summary of Procedures for Elaboration Control
=============================================

.. index:: Elaboration control

First, compile your program with the default options, using none of
the special elaboration-control switches. If the binder successfully
binds your program, then you can be confident that, apart from issues
raised by the use of access-to-subprogram types and dynamic dispatching,
the program is free of elaboration errors. If it is important that the
program be portable to other compilers than GNAT, then use the
*-gnatel*
switch to generate messages about missing `Elaborate` or
`Elaborate_All` pragmas, and supply the missing pragmas.

If the program fails to bind using the default static elaboration
handling, then you can fix the program to eliminate the binder
message, or recompile the entire program with the
*-gnatE* switch to generate dynamic elaboration checks,
and, if you are sure there really are no elaboration problems,
use a global pragma `Suppress (Elaboration_Check)`.


.. _Other_Elaboration_Order_Considerations:

Other Elaboration Order Considerations
======================================

This section has been entirely concerned with the issue of finding a valid
elaboration order, as defined by the Ada Reference Manual. In a case
where several elaboration orders are valid, the task is to find one
of the possible valid elaboration orders (and the static model in GNAT
will ensure that this is achieved).

The purpose of the elaboration rules in the Ada Reference Manual is to
make sure that no entity is accessed before it has been elaborated. For
a subprogram, this means that the spec and body must have been elaborated
before the subprogram is called. For an object, this means that the object
must have been elaborated before its value is read or written. A violation
of either of these two requirements is an access before elaboration order,
and this section has been all about avoiding such errors.

In the case where more than one order of elaboration is possible, in the
sense that access before elaboration errors are avoided, then any one of
the orders is 'correct' in the sense that it meets the requirements of
the Ada Reference Manual, and no such error occurs.

However, it may be the case for a given program, that there are
constraints on the order of elaboration that come not from consideration
of avoiding elaboration errors, but rather from extra-lingual logic
requirements. Consider this example:

.. code-block:: ada

     with Init_Constants;
     package Constants is
        X : Integer := 0;
        Y : Integer := 0;
     end Constants;

     package Init_Constants is
        procedure P; --* require a body*
     end Init_Constants;

     with Constants;
     package body Init_Constants is
        procedure P is begin null; end;
     begin
        Constants.X := 3;
        Constants.Y := 4;
     end Init_Constants;

     with Constants;
     package Calc is
        Z : Integer := Constants.X + Constants.Y;
     end Calc;

     with Calc;
     with Text_IO; use Text_IO;
     procedure Main is
     begin
        Put_Line (Calc.Z'Img);
     end Main;

In this example, there is more than one valid order of elaboration. For
example both the following are correct orders::

     Init_Constants spec
     Constants spec
     Calc spec
     Init_Constants body
     Main body

and

::

    Init_Constants spec
    Init_Constants body
    Constants spec
    Calc spec
    Main body

There is no language rule to prefer one or the other, both are correct
from an order of elaboration point of view. But the programmatic effects
of the two orders are very different. In the first, the elaboration routine
of `Calc` initializes `Z` to zero, and then the main program
runs with this value of zero. But in the second order, the elaboration
routine of `Calc` runs after the body of Init_Constants has set
`X` and `Y` and thus `Z` is set to 7 before `Main` runs.

One could perhaps by applying pretty clever non-artificial intelligence
to the situation guess that it is more likely that the second order of
elaboration is the one desired, but there is no formal linguistic reason
to prefer one over the other. In fact in this particular case, GNAT will
prefer the second order, because of the rule that bodies are elaborated
as soon as possible, but it's just luck that this is what was wanted
(if indeed the second order was preferred).

If the program cares about the order of elaboration routines in a case like
this, it is important to specify the order required. In this particular
case, that could have been achieved by adding to the spec of Calc:

.. code-block:: ada

     pragma Elaborate_All (Constants);

which requires that the body (if any) and spec of `Constants`,
as well as the body and spec of any unit |withed| by
`Constants` be elaborated before `Calc` is elaborated.

Clearly no automatic method can always guess which alternative you require,
and if you are working with legacy code that had constraints of this kind
which were not properly specified by adding `Elaborate` or
`Elaborate_All` pragmas, then indeed it is possible that two different
compilers can choose different orders.

However, GNAT does attempt to diagnose the common situation where there
are uninitialized variables in the visible part of a package spec, and the
corresponding package body has an elaboration block that directly or
indirectly initialized one or more of these variables. This is the situation
in which a pragma Elaborate_Body is usually desirable, and GNAT will generate
a warning that suggests this addition if it detects this situation.

The `gnatbind` *-p* switch may be useful in smoking
out problems. This switch causes bodies to be elaborated as late as possible
instead of as early as possible. In the example above, it would have forced
the choice of the first elaboration order. If you get different results
when using this switch, and particularly if one set of results is right,
and one is wrong as far as you are concerned, it shows that you have some
missing `Elaborate` pragmas. For the example above, we have the
following output:

.. code-block:: sh

     $ gnatmake -f -q main
     $ main
      7
     $ gnatmake -f -q main -bargs -p
     $ main
      0

It is of course quite unlikely that both these results are correct, so
it is up to you in a case like this to investigate the source of the
difference, by looking at the two elaboration orders that are chosen,
and figuring out which is correct, and then adding the necessary
`Elaborate` or `Elaborate_All` pragmas to ensure the desired order.


.. _Determining_the_Chosen_Elaboration_Order:

Determining the Chosen Elaboration Order
========================================

To see the elaboration order that the binder chooses, you can look at
the last part of the file:`b~xxx.adb` binder output file. Here is an example::

     System.Soft_Links'Elab_Body;
     E14 := True;
     System.Secondary_Stack'Elab_Body;
     E18 := True;
     System.Exception_Table'Elab_Body;
     E24 := True;
     Ada.Io_Exceptions'Elab_Spec;
     E67 := True;
     Ada.Tags'Elab_Spec;
     Ada.Streams'Elab_Spec;
     E43 := True;
     Interfaces.C'Elab_Spec;
     E69 := True;
     System.Finalization_Root'Elab_Spec;
     E60 := True;
     System.Os_Lib'Elab_Body;
     E71 := True;
     System.Finalization_Implementation'Elab_Spec;
     System.Finalization_Implementation'Elab_Body;
     E62 := True;
     Ada.Finalization'Elab_Spec;
     E58 := True;
     Ada.Finalization.List_Controller'Elab_Spec;
     E76 := True;
     System.File_Control_Block'Elab_Spec;
     E74 := True;
     System.File_Io'Elab_Body;
     E56 := True;
     Ada.Tags'Elab_Body;
     E45 := True;
     Ada.Text_Io'Elab_Spec;
     Ada.Text_Io'Elab_Body;
     E07 := True;

Here Elab_Spec elaborates the spec
and Elab_Body elaborates the body. The assignments to the :samp:`E{xx}` flags
flag that the corresponding body is now elaborated.

You can also ask the binder to generate a more
readable list of the elaboration order using the
`-l` switch when invoking the binder. Here is
an example of the output generated by this switch::

     ada (spec)
     interfaces (spec)
     system (spec)
     system.case_util (spec)
     system.case_util (body)
     system.concat_2 (spec)
     system.concat_2 (body)
     system.concat_3 (spec)
     system.concat_3 (body)
     system.htable (spec)
     system.parameters (spec)
     system.parameters (body)
     system.crtl (spec)
     interfaces.c_streams (spec)
     interfaces.c_streams (body)
     system.restrictions (spec)
     system.restrictions (body)
     system.standard_library (spec)
     system.exceptions (spec)
     system.exceptions (body)
     system.storage_elements (spec)
     system.storage_elements (body)
     system.secondary_stack (spec)
     system.stack_checking (spec)
     system.stack_checking (body)
     system.string_hash (spec)
     system.string_hash (body)
     system.htable (body)
     system.strings (spec)
     system.strings (body)
     system.traceback (spec)
     system.traceback (body)
     system.traceback_entries (spec)
     system.traceback_entries (body)
     ada.exceptions (spec)
     ada.exceptions.last_chance_handler (spec)
     system.soft_links (spec)
     system.soft_links (body)
     ada.exceptions.last_chance_handler (body)
     system.secondary_stack (body)
     system.exception_table (spec)
     system.exception_table (body)
     ada.io_exceptions (spec)
     ada.tags (spec)
     ada.streams (spec)
     interfaces.c (spec)
     interfaces.c (body)
     system.finalization_root (spec)
     system.finalization_root (body)
     system.memory (spec)
     system.memory (body)
     system.standard_library (body)
     system.os_lib (spec)
     system.os_lib (body)
     system.unsigned_types (spec)
     system.stream_attributes (spec)
     system.stream_attributes (body)
     system.finalization_implementation (spec)
     system.finalization_implementation (body)
     ada.finalization (spec)
     ada.finalization (body)
     ada.finalization.list_controller (spec)
     ada.finalization.list_controller (body)
     system.file_control_block (spec)
     system.file_io (spec)
     system.file_io (body)
     system.val_uns (spec)
     system.val_util (spec)
     system.val_util (body)
     system.val_uns (body)
     system.wch_con (spec)
     system.wch_con (body)
     system.wch_cnv (spec)
     system.wch_jis (spec)
     system.wch_jis (body)
     system.wch_cnv (body)
     system.wch_stw (spec)
     system.wch_stw (body)
     ada.tags (body)
     ada.exceptions (body)
     ada.text_io (spec)
     ada.text_io (body)
     text_io (spec)
     gdbstr (body)
