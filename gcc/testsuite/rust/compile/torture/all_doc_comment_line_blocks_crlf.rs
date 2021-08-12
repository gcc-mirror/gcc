// comment line not a doc
/* comment block not a doc                   */

//! inner line comment for most outer crate
/*! inner block comment for most outer crate */

// comment line not a doc
/* comment block not a doc                   */

/// outer doc line for module
/** outer doc block for module               */
pub mod module // { dg-warning "unused name" }
{
  //!  inner line doc
  //!! inner line doc!
  /*!  inner block doc  */
  /*!! inner block doc! */

  //   line comment
  ///  outer line doc
  //// line comment

  /*   block comment   */
  /**  outer block doc */
  /*** block comment   */

  mod block_doc_comments // { dg-warning "unused name" }
  {
    /*   /* */  /** */  /*! */  */
    /*!  /* */  /** */  /*! */  */
    /**  /* */  /** */  /*! */  */
    mod item { } // { dg-warning "unused name" }
  }

  pub mod empty // { dg-warning "unused name" }
  {
    //!
    /*!*/
    //

    ///
    mod doc { }    // { dg-warning "unused name" }

    /**/
    /***/
  }
}
pub fn main () { }
