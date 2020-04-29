#ifndef RUST_AST_CONTAINERS_H
#define RUST_AST_CONTAINERS_H
// crappy redefined AST maybe. may move

/* This is mainly the "logical", more "abstract" representation of the code,
 * while the "AST" itself is more high-level and matches the language better. */

// this is now deprecated and replaced with the proper AST
#error "rust-ast-containers.h was included by accident. Don't use."

namespace Rust {
namespace AST {
struct Module
{
public:
};

struct Crate
{
public:
  Module root_module;
};

// replace with rust-types.h version
struct AttributeList
{
public:
  //::std::vector<Attribute> attribs;
};

// replace with rust-types.h version
struct Visibility
{
};

/*enum VisibilityType {
    Private,
    PublicFull,
    PublicInPath,
    PublicCrate,
    PublicSuper,
    PublicSelfModule
};

// Represents visibility - maybe make into an enum or union or something
struct Visibility {
};

*/
} // namespace AST
} // namespace Rust

#endif
