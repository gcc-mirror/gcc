/* Loosely-coupled notifications via the Publish-Subscribe pattern.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_PUB_SUB_H
#define GCC_PUB_SUB_H

namespace pub_sub {

template <typename Subscriber>
class channel
{
public:
  using subscriber = Subscriber;

  // A node within the std::list
  using subscription = typename std::list<subscriber *>::iterator;

  /* Return this if this channel has subscribers, or nullptr if
     there are none.  */
  const channel *
  get_if_active () const
  {
    if (m_subscribers.empty ())
      return nullptr;
    return this;
  }

  template <typename Message>
  void publish (const Message &m) const
  {
    for (auto sub : m_subscribers)
      sub->on_message (m);
  }

  subscription
  add_subscriber (subscriber &s)
  {
    return m_subscribers.insert (m_subscribers.end (), &s);
  }
  void unsubscribe (subscription s)
  {
    m_subscribers.remove (s);
  }

private:
  std::list<subscriber *> m_subscribers;
};

} // namespace pub_sub

#endif /* GCC_PUB_SUB_H */
