// PR c++/123684
// { dg-do compile { target c++11 } }

inline namespace [[foo::abi_tag (1, 2, 3)]] A {}	// { dg-warning "'abi_tag' attribute directive ignored" }
inline namespace [[foo::visibility (1, 2, 3)]] B {}	// { dg-warning "'visibility' attribute directive ignored" }
inline namespace [[foo::deprecated (1, 2, 3)]] C {}	// { dg-warning "'deprecated' attribute directive ignored" }
inline namespace [[abi_tag (1, 2, 3)]] D {}		// { dg-warning "'abi_tag' attribute directive ignored" }
inline namespace [[visibility (1, 2, 3)]] E {}		// { dg-warning "'visibility' attribute directive ignored" }
