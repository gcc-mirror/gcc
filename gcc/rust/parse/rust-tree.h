#ifndef RUST_TREE_H
#define RUST_TREE_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-iterator.h"
#include "input.h"
// order: config, system, coretypes, tree, tree-iterator, input
// may not need all of them

namespace Rust {
    // Wrapper around tree to keep location and tree in one data structure.
    struct Tree {
      public:
        Tree() : t(NULL_TREE), loc(UNKNOWN_LOCATION) {}
        Tree(tree t_) : t(t_), loc(EXPR_LOCATION(t)) {}
        Tree(tree t_, location_t loc_) : t(t_), loc(loc_) {}
        Tree(Tree t_, location_t loc_) : t(t_.get_tree()), loc(loc_) {}

        // Get tree's location_t.
        location_t get_locus() const {
            return loc;
        }

        // Sets tree's location_t.
        void set_locus(location_t loc_) {
            loc = loc_;
        }

        // Gets tree's tree in GCC form.
        tree get_tree() const {
            return t;
        }

        // Gets tree's GCC tree code.
        tree_code get_tree_code() const {
            return TREE_CODE(t);
        }

        // Sets tree's GCC tree.
        void set_tree(tree t_) {
            t = t_;
        }

        // Returns if tree is an error node?
        bool is_error() const {
            return error_operand_p(t);
        }

        // Returns whether tree node is null.
        bool is_null() {
            return t == NULL_TREE;
        }

        // Creates an error Tree.
        static Tree error() {
            return Tree(error_mark_node);
        }

        // Gets tree's GCC type.
        Tree get_type() const {
            return TREE_TYPE(t);
        }

      private:
        // The tree object's gcc tree representation.
        tree t;
        // The tree's location.
        location_t loc;
    };

    // Comparison by identity as tree is a pointer.
    inline bool operator==(Tree t1, Tree t2) {
        return t1.get_tree() == t2.get_tree();
    }
    inline bool operator!=(Tree t1, Tree t2) {
        return !(t1 == t2);
    }

    inline Tree build_tree(tree_code tc, location_t loc, Tree type, Tree t1) {
        return build1_loc(loc, tc, type.get_tree(), t1.get_tree());
    }

    inline Tree build_tree(tree_code tc, location_t loc, Tree type, Tree t1, Tree t2) {
        return build2_loc(loc, tc, type.get_tree(), t1.get_tree(), t2.get_tree());
    }

    inline Tree build_tree(tree_code tc, location_t loc, Tree type, Tree t1, Tree t2, Tree t3) {
        return build3_loc(loc, tc, type.get_tree(), t1.get_tree(), t2.get_tree(), t3.get_tree());
    }

    inline Tree build_tree(
      tree_code tc, location_t loc, Tree type, Tree t1, Tree t2, Tree t3, Tree t4) {
        return build4_loc(
          loc, tc, type.get_tree(), t1.get_tree(), t2.get_tree(), t3.get_tree(), t4.get_tree());
    }

    inline Tree build_tree(
      tree_code tc, location_t loc, Tree type, Tree t1, Tree t2, Tree t3, Tree t4, Tree t5) {
        return build5_loc(loc, tc, type.get_tree(), t1.get_tree(), t2.get_tree(), t3.get_tree(),
          t4.get_tree(), t5.get_tree());
    }

    // Wrapper around STATEMENT_LIST, used to represent lists of statements. Adapter for TREE_LIST.
    struct TreeStmtList {
      public:
        // Create new statement list from nothing.
        TreeStmtList() : list(alloc_stmt_list()) {}
        // Create new statement list from given tree.
        TreeStmtList(Tree param_tree) : list(param_tree.get_tree()) {}

        // Append to statement list.
        void append(Tree param_tree) {
            append_to_statement_list(param_tree.get_tree(), &list);
        }

        // Get the statement list.
        tree get_tree() const {
            return list;
        }

      private:
        // The statement list.
        tree list;
    };

    // TODO: Check if already exists in GCC
    template<typename Append>
    struct TreeChainBase {
        Tree first;
        Tree last;

        TreeChainBase() : first(), last() {}

        void append(Tree t) {
            gcc_assert(!t.is_null());
            if (first.is_null()) {
                first = last = t;
            } else {
                Append()(last, t);
                last = t;
            }
        }
    };

    struct tree_chain_append {
        void operator()(Tree t, Tree a) {
            TREE_CHAIN(t.get_tree()) = a.get_tree();
        }
    };

    // Single-linked list implemented with trees. Used for VAR_DECLs.
    struct TreeChain : TreeChainBase<tree_chain_append> {};

    struct block_chain_append {
        void operator()(Tree t, Tree a) {
            BLOCK_CHAIN(t.get_tree()) = a.get_tree();
        }
    };

    // Single-linked list implemented with trees. Used for chains of blocks.
    struct BlockChain : TreeChainBase<block_chain_append> {};
}

#endif // RUST_TREE_H