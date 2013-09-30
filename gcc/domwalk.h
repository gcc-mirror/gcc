/* Generic dominator tree walker
   Copyright (C) 2003-2013 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DOM_WALK_H
#define GCC_DOM_WALK_H

/**
 * This is the main class for the dominator walker. It is expected that
 * consumers will have a custom class inheriting from it, which will over ride
 * at least one of before_dom_children and after_dom_children to implement the
 * custom behavior.
 */
class dom_walker
{
public:
  dom_walker (cdi_direction direction) : m_dom_direction (direction) {}

  /* Walk the dominator tree.  */
  void walk (basic_block);

  /* Function to call before the recursive walk of the dominator children.  */
  virtual void before_dom_children (basic_block) {}

  /* Function to call after the recursive walk of the dominator children.  */
  virtual void after_dom_children (basic_block) {}

private:
  /* This is the direction of the dominator tree we want to walk.  i.e.,
     if it is set to CDI_DOMINATORS, then we walk the dominator tree,
     if it is set to CDI_POST_DOMINATORS, then we walk the post
     dominator tree.  */
  const ENUM_BITFIELD (cdi_direction) m_dom_direction : 2;
};

#endif
