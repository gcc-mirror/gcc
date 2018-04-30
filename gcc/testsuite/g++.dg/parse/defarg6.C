// { dg-do compile }

// Origin: Falk Hueffner <falk@debian.org>

// PR c++/13166: ICE default function argument for friend declaration.

namespace sc_dt {
    class sc_length_param {
	friend int compare_unsigned(int if_v_signed = 0) { return 0; }
    };
}
