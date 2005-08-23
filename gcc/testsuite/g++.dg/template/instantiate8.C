// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Aug 2005 <nathan@codesourcery.com>

// PR 22044: ICE
// Origin: Andrew Pinski <pinskia@gcc.gnu.org>

struct no_context {
  template< class Event > void no_function( const Event & );
};
template< class Event, class TransitionContext = no_context,
void ( TransitionContext::*pTransitionAction )( const Event & ) = &no_context::no_function< Event > >
struct transition
{
  struct EvFlipBit {};
  typedef transition<EvFlipBit> type;
};

